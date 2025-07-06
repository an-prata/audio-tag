use crate::{audio_info::Audio, id3v2};
use extended::Extended;
use std::{
    error::Error,
    fmt::Display,
    fs,
    io::{self, Write},
    path::Path,
    result,
};

/// An AIFF file (FORM AIFF).
#[derive(Debug, Clone)]
pub struct File {
    /// The FORM AIFF. This [`FormChunk`] should be validated and is guaranteed to contain the
    /// [`Chunk`]s required by AIFF 1.3.
    ///
    /// [`Chunk`]: Chunk
    /// [`FormChunk`]: FormChunk
    form_aiff: FormChunk,
}

impl File {
    /// Open and parse an AIFF (FORM AIFF) file.
    pub fn read_from(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|e| ParseError::Io(e))?;
        let mut top_level_chunks: Vec<Chunk> = parse_chunks(&bytes).collect();

        if top_level_chunks.len() > 1 {
            return Err(ParseError::ExcessTopLevelChunks);
        }

        match top_level_chunks.pop() {
            Some(Chunk::Form(form_chunk)) => {
                validate_form_aiff(&form_chunk)?;
                Ok(File {
                    form_aiff: form_chunk,
                })
            }

            _ => Err(ParseError::ExpectedFormChunk),
        }
    }

    /// Write this [`aiff::File`] to a file at the given [`AsRef<Path>`], creating it if it does not
    /// already exist.
    ///
    /// [`aiff::File`]: File
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn write_to(self, path: impl AsRef<Path>) -> Result<()> {
        let bytes = Chunk::Form(self.form_aiff).untype().to_bytes();
        let mut file = fs::File::create(path).map_err(|io_err| ParseError::Io(io_err))?;
        file.write_all(&bytes)
            .map_err(|io_err| ParseError::Io(io_err))?;

        Ok(())
    }

    /// Get this FORM AIFF file's [`CommonChunk`].
    ///
    /// [`CommonChunk`]: CommonChunk
    fn common_chunk(&self) -> CommonChunk {
        // When constructing the `aiff::File` we validate that this `CommonChunk` exists, so
        // this function should never panic.
        self.form_aiff
            .chunks
            .iter()
            .find_map(|chunk| match chunk {
                Chunk::Common(common_chunk) => Some(*common_chunk),
                _ => None,
            })
            .unwrap()
    }

    /// Find the [`Id3v2Chunk`] and return the [`id3v2::Tag`] that it contains if successful.
    ///
    /// [`Id3v2Chunk`]: Id3v2Chunk
    /// [`id3v2::Tag`]: id3v2::Tag
    fn tag(&self) -> Option<&id3v2::Tag> {
        self.form_aiff.chunks.iter().find_map(|chunk| match chunk {
            Chunk::Id3v2(tag_chunk) => Some(&tag_chunk.tag),
            _ => None,
        })
    }
}

impl Audio for File {
    fn sample_rate(&self) -> f64 {
        self.common_chunk().sample_rate.to_f64()
    }

    fn sample_size(&self) -> u16 {
        self.common_chunk().sample_size as _
    }

    fn channels(&self) -> u16 {
        self.common_chunk().channels as _
    }
}

/// Type alias for [`Result`] prefilled with a [`ParseError`] for the error type.
///
/// [`Result`]: result::Result
/// [`ParseError`]: ParseError
pub type Result<T> = result::Result<T, ParseError>;

/// Errors that may occure when opening/parsing an AIFF file.
#[derive(Debug)]
pub enum ParseError {
    /// An IO error.
    Io(io::Error),

    /// Too many top level chunks. AIFF files should have a single top level FORM chunk.
    ExcessTopLevelChunks,

    /// Could not find a FORM chunk when one was expected.
    ExpectedFormChunk,

    /// Did not expect a FORM chunk but one was found.
    UnexpectedFormChunk,

    /// Expected a common chunk but none were found.
    ExpectedCommonChunk,

    /// Too many common chunks present. An AIFF may only have one common chunk.
    TooManyCommonChunks,

    /// There was no sound data chunk but the number of samples was not zero.
    ExpectedSoundChunk,

    /// Too many sound data chunks present. An AIFF may only have one sound data chunk.
    TooManySoundChunks,

    /// Too many ID3 chunks, expected only one.
    TooManyId3Chunks,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Io(error) => write!(f, "could not parse AIFF: IO error: {}", error),
            ParseError::ExcessTopLevelChunks => write!(
                f,
                "too many top level chunks, AIFF files should have a single top level FORM chunk"
            ),
            ParseError::ExpectedFormChunk => {
                write!(f, "could not find a FORM chunk when one was expected")
            }
            ParseError::UnexpectedFormChunk => {
                write!(f, "did not expect a FORM chunk but one was found")
            }
            ParseError::ExpectedCommonChunk => {
                write!(f, "expected a common chunk but none were found")
            }
            ParseError::TooManyCommonChunks => write!(
                f,
                "too many common chunks present, an AIFF may only have one common chunk"
            ),
            ParseError::ExpectedSoundChunk => write!(
                f,
                "number of samples was not zero but no sound data chunk was present"
            ),
            ParseError::TooManySoundChunks => write!(
                f,
                "too many sound data chunks present, an AIFF may only have one sound data chunk"
            ),
            ParseError::TooManyId3Chunks => write!(f, "too many ID3 chunks, expected only one"),
        }
    }
}

/// Contains all the possible chunk types that this module will use.
#[derive(Debug, Clone)]
enum Chunk {
    /// A chunk without a currently parsed type. This chunk includes a four byte/character ID and
    /// raw data in the form of a [`Vec<u8>`].
    ///
    /// [`Vec<u8>`]: Vec<u8>
    Typeless(TypelessChunk),

    /// A "FORM" chunk, which wraps the other chunks in an AIFF file.
    Form(FormChunk),

    /// A common chunk contains basic and necessary information about the sound data in an AIFF
    /// file.
    Common(CommonChunk),

    /// The AIFF's actuall sound data, which includes samples as per the common chunk's spec.
    SoundData(SoundDataChunk),

    /// Contains all the AIFF's markers, which can add info/text to certain points between samples.
    Marker(MarkerChunk),

    /// An instrument chunk, containing information about how an intrument should sound.
    Instrument(InstrumentChunk),

    /// Contains MIDI data.
    MidiData(MidiDataChunk),

    /// Contains information about/for audio recording devices.
    AudioRecording(AudioRecordingChunk),

    /// Application specific chunk, who's spec is determined by the application using it.
    ApplicationSpecific(ApplicationSpecificChunk),

    /// Contains textual comments.
    Comments(CommentsChunk),

    /// Contains the name of the track/audio.
    Name(NameChunk),

    /// Contains the author/creator of the sound's name.
    Author(AuthorChunk),

    /// Contains the copyright message, which is what would come immediately after "©". e.g. "1988
    /// Apple Computer, Inc." means "© 1988 Apple Computer, Inc.".
    Copyright(CopyrightChunk),

    /// Contains a comment, though its use is discourages in favor of a [`CommentsChunk`].
    ///
    /// [`CommentsChunk`]: CommentsChunk
    Annotation(AnnotationChunk),

    /// Not in the AIFF 1.3 spec, but this chunk holds ID3v2 tag info about the track.
    Id3v2(Id3v2Chunk),
}

impl Chunk {
    /// Discard the typing of the given [`Chunk`] and give a [`TypelessChunk`] containing the same
    /// data/bytes.
    ///
    /// [`Chunk`]: Chunk
    /// [`TypelessChunk`]: TypelessChunk
    fn untype(self) -> TypelessChunk {
        match self {
            Chunk::Typeless(typeless_chunk) => typeless_chunk,

            Chunk::Form(form_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut form_chunk.form_type.into());
                bytes.append(
                    &mut form_chunk
                        .chunks
                        .into_iter()
                        .flat_map(|c| c.untype().to_bytes())
                        .collect(),
                );

                TypelessChunk {
                    id: ID_FORM,
                    data: bytes,
                }
            }

            Chunk::Common(common_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut common_chunk.channels.to_be_bytes().into());
                bytes.append(&mut common_chunk.num_sample_frames.to_be_bytes().into());
                bytes.append(&mut common_chunk.sample_size.to_be_bytes().into());
                bytes.append(&mut common_chunk.sample_rate.to_be_bytes().into());

                TypelessChunk {
                    id: ID_COMMON,
                    data: bytes,
                }
            }

            Chunk::SoundData(mut sound_data_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut sound_data_chunk.offset.to_be_bytes().into());
                bytes.append(&mut sound_data_chunk.block_size.to_be_bytes().into());
                bytes.append(&mut sound_data_chunk.sound_data);

                TypelessChunk {
                    id: ID_SOUND_DATA,
                    data: bytes,
                }
            }

            Chunk::Marker(mut marker_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut marker_chunk.num_markers.to_be_bytes().into());
                bytes.append(&mut marker_chunk.markers);

                TypelessChunk {
                    id: ID_MARKER,
                    data: bytes,
                }
            }

            Chunk::Instrument(instrument_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut instrument_chunk.base_note.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.detune.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.low_note.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.high_note.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.low_velocity.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.high_velocity.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.gain.to_be_bytes().into());
                bytes.append(&mut instrument_chunk.sustain_loop.to_bytes());
                bytes.append(&mut instrument_chunk.release_loop.to_bytes());

                TypelessChunk {
                    id: ID_INSTRUMENT,
                    data: bytes,
                }
            }

            Chunk::MidiData(midi_data_chunk) => TypelessChunk {
                id: ID_MIDI_DATA,
                data: midi_data_chunk.midi_data,
            },

            Chunk::AudioRecording(audio_recording_chunk) => TypelessChunk {
                id: ID_AUDIO_RECORDING,
                data: audio_recording_chunk.aes_channel_status_data.into(),
            },

            Chunk::ApplicationSpecific(mut application_specific_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut application_specific_chunk.application_signature.to_vec());
                bytes.append(&mut application_specific_chunk.data);

                TypelessChunk {
                    id: ID_APPLICATION_SPECIFIC,
                    data: bytes,
                }
            }

            Chunk::Comments(mut comments_chunk) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut comments_chunk.num_comments.to_be_bytes().into());
                bytes.append(&mut comments_chunk.comments);

                TypelessChunk {
                    id: ID_COMMENTS,
                    data: bytes,
                }
            }

            Chunk::Name(name_chunk) => TypelessChunk {
                id: ID_NAME,
                data: name_chunk.text,
            },

            Chunk::Author(author_chunk) => TypelessChunk {
                id: ID_AUTHOR,
                data: author_chunk.text,
            },

            Chunk::Copyright(copyright_chunk) => TypelessChunk {
                id: ID_COPYRIGHT,
                data: copyright_chunk.text,
            },

            Chunk::Annotation(annotation_chunk) => TypelessChunk {
                id: ID_ANNOTATION,
                data: annotation_chunk.text,
            },

            Chunk::Id3v2(id3v2_chunk) => TypelessChunk {
                id: ID_ID3V2,
                data: id3v2_chunk.tag.to_bytes(),
            },
        }
    }
}

/// Creates an [`Iterator`] over all chunks in a given slice of bytes.
fn parse_chunks<'d>(bytes: &'d [u8]) -> ChunksIter<'d> {
    ChunksIter {
        remaining_bytes: bytes,
    }
}

/// An [`Iterator`] which yields [`Chunk`]s as they come parsed out of raw bytes.
///
/// [`Iterator`]: Iterator
/// [`Chunk`]: Chunk
struct ChunksIter<'d> {
    /// The not yet parsed bytes left.
    remaining_bytes: &'d [u8],
}

impl<'d> Iterator for ChunksIter<'d> {
    type Item = Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        let (chunk, remaining_bytes) = TypelessChunk::from_bytes(self.remaining_bytes)?;
        self.remaining_bytes = remaining_bytes;
        Some(chunk.typed()?)
    }
}

/// Generic chunk from an AIFF file. All other chunk types will ommit the `id` field as it is the
/// indicator of the chunk's type, and becomes redundant with specialized data types.
#[derive(Debug, Clone)]
struct TypelessChunk {
    /// "ckID" in AIFF 1.3 documentation, this field should be used to interprent the meaning of the
    /// [`Chunk`]'s data.
    ///
    /// [`Chunk`]: Chunk
    id: [u8; 4],

    /// "ckData" in AIFF 1.3 documentation, this field is the [`Chunk`]'s actual data. This field
    /// should be interpreted differently based on the value of `id`.
    ///
    /// [`Chunk`]: Chunk
    data: Vec<u8>,
}

/// A special chunk, which encompasses all others in an AIFF file.
#[derive(Debug, Clone)]
struct FormChunk {
    /// The FORM chunk's type. For an AIFF file this should be `b"AIFF"`.
    form_type: [u8; 4],

    /// The remaining `data` from the original [`Chunk`]. For a FORM chunk this is the remaining
    /// [`Chunk`]s of the file.
    ///
    /// [`Chunk`]: Chunk
    chunks: Vec<Chunk>,
}

/// The COMM or common chunk which describes properties of the audio. One and only one common chunk
/// is required and allowed in a given FORM chunk.
#[derive(Debug, Clone, Copy)]
struct CommonChunk {
    /// The number of channels in the audio, 1 would mono, 2 would be stereo, 6 would be 5.1 suround
    /// sound, so on and so forth. Any number of channels is permmited.
    ///
    /// For multichannel audio sound frames are interleaved, such that the first channel's frame
    /// comes first, then the second channel's, and so on, until a complete frame is represented,
    /// at which point the next frame for the whole multi-channel audio begins. Like so:
    ///
    /// Frame 1:          Frame 2:          Frame 3:
    /// | ch 1 | ch 2 |   | ch 1 | ch 2 |   | ch 1 | ch 2 |
    channels: i16,

    /// The total number of sample frams of the entire multi-channel audio. Note that this is not
    /// the number of sample _points_, which would be the number of sample frames times the number
    /// of channels.
    num_sample_frames: u32,

    /// The number of bits (not bytes) for each sample point (not frame). May be any number from 1
    /// to 32.
    sample_size: i16,

    /// The rate at which sound is sampled, in sample frames (not points) per second.
    sample_rate: Extended,
}

/// Sound data chunk which contains actual sample frames. This chunk is required unless the
/// `num_sample_frames` field in the [`CommonChunk`] is set to 0.
///
/// [`CommonChunk`]: CommonChunk
#[derive(Debug, Clone)]
struct SoundDataChunk {
    /// The offset of block-aligned data. This is a number of bytes which should be skipped in
    /// `sound_data` before attempting to read samples. An offset of zero indicates that samples
    /// begin immediately in `sound_data`.
    offset: u32,

    /// A `block_size` of zero indicates that `sound_data` need not be block aligned. A block size
    /// greater than zero indicates that a sample frame should take up `block_size` bytes, padded as
    /// needed, within sound_data. For example, a `block_size` of 4 would indicate that a 16 bit
    /// sample should be followed by 16 zero bits of padding.
    block_size: u32,

    /// Sample points are stored as left justified two's compliment values. Their size is determined
    /// by the `sample_size` field of the [`CommonChunk`]. Samples should take up whole bytes, so
    /// sizes 1 to 8 bites should take a byte, sizes 9 to 16 should take two bytes, sizes 17 to 24
    /// should take up three bytes, and sizes 25 to 32 should take up 4 bytes. Sizes greater than 32
    /// bits are invalid.
    ///
    /// Sample frames are stored contiguosly in order of increasing time. The sample points within a
    /// sample frame are packed together and contain no padding bytes. Similarly sample frames
    /// themselves are packed together and have no padding between them.
    ///
    /// [`CommonChunk`]: CommonChunk
    sound_data: Vec<u8>,
}

/// A marker chunk is optional, and only one may appear in the file.
#[derive(Debug, Clone)]
struct MarkerChunk {
    /// The number of markers this chunk contains.
    num_markers: u16,

    /// Data for markers.
    markers: Vec<u8>,
}

/// Instrument chunks are optional and no more than one may appear in a FORM AIFF.
#[derive(Debug, Clone, Copy)]
struct InstrumentChunk {
    /// The base MIDI note that the instrument plays. A value of 60 is middle C, values 0 through
    /// 127 are valid.
    base_note: u8,

    /// Determines the extent to which the base note should have its pitch modified. Unit is cents
    /// (a hundredth of a semitone), values -50 through 50 are accepted. A negative value indicates
    /// reducing pitch, a positive value indicates raising pitch.
    detune: u8,

    /// The low note of the suggested range on a keyboard for playback of the sound data. The sound
    /// data should be played if the instrument is requested to play a note between the low and high
    /// note inclusively. Unit is MIDI note values. The base note need not be in this range.
    low_note: u8,

    /// The hight note of the suggested range on a keyboard for playback of the sound data. The
    /// sound data should be played if the instrument is requested to play a note between the low
    /// and high note inclusively. Unit is MIDI note values. The base note need not be in this
    /// range.
    high_note: u8,

    /// The lower bound of the suggested range of velocities for playback of sound data. The sound
    /// data should be played if the note-on velocity is between the lower bound and upper bound of
    /// valocity. Unites are MIDI velocity values in the range 1 to 127 inclusive where 1 is the
    /// lowest velocity and 127 is the highest.
    low_velocity: u8,

    /// The upper bound of the suggested range of velocities for playback of sound data. The sound
    /// data should be played if the note-on velocity is between the lower bound and upper bound of
    /// valocity. Unites are MIDI velocity values in the range 1 to 127 inclusive where 1 is the
    /// lowest velocity and 127 is the highest.
    high_velocity: u8,

    /// The amount to change the gain of the sound when played. Unit is decibels, where 0 is no
    /// change, negative values decrease the gain, and positive values increase gain. 6 db would
    /// double the value of each sample point, -6 db would half the value of each sample point.
    gain: i16,

    /// The loop to play when the instrument plays a sustained note.
    sustain_loop: Loop,

    /// The loop to play when the instrument is in the release phase of playing back a sound. This
    /// typically occurs after the key that played the note is released.
    release_loop: Loop,
}

/// A chunk for storing MIDI data. Any number of these chunks may exist, including none, within the
/// FORM AIFF.
#[derive(Debug, Clone)]
struct MidiDataChunk {
    /// A stream of MIDI data.
    midi_data: Vec<u8>,
}

/// Audio recording chunk which contains pertinent information to audio recording devices.
#[derive(Debug, Clone, Copy)]
struct AudioRecordingChunk {
    aes_channel_status_data: [u8; 24],
}

/// Contains data specific to a given application. May be used for any purpose and is optional. Any
/// number of application specific chunks are allowed.
#[derive(Debug, Clone)]
struct ApplicationSpecificChunk {
    /// Identifies the particular application. For Macintosh applications this is the application's
    /// four character signature. For Apple II applications the signature should always be 'pdos'.
    /// If `application_signature` is 'pdos' the beginning of data is defined as a Pasal-style
    /// ShortString containing the name of the application. This is needed as Apple II applications
    /// do not have four-byte signatures like Macintosh applications.
    application_signature: OsType,

    /// Application specified data.
    data: Vec<u8>,
}

/// Chunk containing all comments.
#[derive(Debug, Clone)]
struct CommentsChunk {
    /// The number of comments within
    num_comments: u16,

    /// Data containing [`Comment`]s.
    ///
    /// [`Comment`]: Comment
    comments: Vec<u8>,
}

/// Chunk containing the name of the sampled sounds. No more than one may exist in a FORM AIFF.
#[derive(Debug, Clone)]
struct NameChunk {
    /// A list of ASCII characters for the text of this chunk.
    text: Vec<u8>,
}

/// Chunk containing one or more author names. Author should be the creator of the sampled sound.
/// No more than one may exist in a FORM AIFF.
#[derive(Debug, Clone)]
struct AuthorChunk {
    /// A list of ASCII characters for the text of this chunk.
    text: Vec<u8>,
}

/// Contains copyright notice for the sound. The chunk ID serves as the copyright sign, so a
/// [`CopyrightChunk`] with the text "1988 Apple Computer, Inc." means "© 1988 Apple Computer,
/// Inc.". No more than one may exist in a FORM AIFF.
///
/// [`CopyrightChunk`]: CopyrightChunk
#[derive(Debug, Clone)]
struct CopyrightChunk {
    /// A list of ASCII characters for the text of this chunk.
    text: Vec<u8>,
}

/// Annotation chunk contains a comment. Use is discouraged in favor of the [`CommentsChunk`]. Many
/// annotation chunks may exist in a FORM AIFF. This chunk is optional.
///
/// [`CommentsChunk`]: CommentsChunk
#[derive(Debug, Clone)]
struct AnnotationChunk {
    /// A list of ASCII characters for the text of this chunk.
    text: Vec<u8>,
}

/// The ID3v2 chunk is the only chunk not described in Apple's FORM AIFF 1.3 documentation. The
/// ID3v2 chunk holds metadata commonly used for things like a track's album, track number, etc.
#[derive(Debug, Clone)]
struct Id3v2Chunk {
    /// A fully parsed ID3v2 [`Tag`].
    ///
    /// [`Tag`]: id3v2::Tag
    tag: id3v2::Tag,
}

/// Type alias for marker IDs.
type MarkerId = i16;

/// Type alias for the `OSType` defined in _Inside Macintosh, vol II_, which is used by AIFF.
type OsType = [u8; 4];

/// A marker, associated with a position in the sound data, which contains some comment/text. Used
/// by the [`MarkerChunk`].
///
/// [`MarkerChunk`]: MarkerChunk
#[derive(Debug, Clone, Copy)]
struct Marker<'s> {
    /// Uniqie identifier for the marker, may be any positive non-zero integer.
    marker_id: MarkerId,

    /// The marker's position, with a unit of frames. A position of 0 indicated a marker which
    /// precedes the first frame. A position of 1 indicated coming after the first frame.
    position: u32,

    /// The marker's name.
    text: PascalShortString<'s>,
}

/// A Pascal-style "ShortString".
#[derive(Debug, Clone, Copy)]
struct PascalShortString<'s> {
    /// AIFF prescribes Pascal-style strings in some areas, where the leading byte is the length of
    /// the string.
    pstring: &'s [u8],
}

/// An instrument loop.
#[derive(Debug, Clone, Copy)]
struct Loop {
    /// The type of looping to be performed. If [`LoopPlayMode::NoLooping`] is selected then the
    /// `begin_loop` and `end_loop` fields are ignored.
    ///
    /// [`LoopPlayMode::NoLooping`]: LoopPlayMode::NoLooping
    play_mode: LoopPlayMode,

    /// ID to the marker which marks the beginning of the loop.
    begin_loop: MarkerId,

    /// ID to the marker which marks the end of the loop.
    end_loop: MarkerId,
}

#[derive(Debug, Clone, Copy)]
enum LoopPlayMode {
    NoLooping,
    ForwardLooping,
    ForwardBackwardLooping,
}

/// Actual comment as stored in a [`CommentsChunk`].
///
/// [`CommentsChunk`]: CommentsChunk
#[derive(Debug, Clone, Copy)]
struct Comment<'t> {
    /// Time stamp in seconds since January 1, 1904 (this is the convention used by the Macintosh).
    time_stamp: u32,

    /// A [`Comment`] may be linked to a [`Marker`], allowing applications to store longer
    /// descriptions of [`Marker`]s in [`Comment`]s. If linked to a [`Marker`], `marker` should be
    /// set to the [`Marker`]'s ID, otherwise it should be set to 0.
    ///
    /// [`Marker`]: Marker
    /// [`Comment`]: Comment
    marker: MarkerId,

    /// Encodes both the `count` and `text` fields as documented in the AIFF-1.3 documentation.
    /// `count` is originally 16 bit so the length of this `text` should not exceed the maximum
    /// value of a `u16`.
    text: &'t [u8],
}

/// Type alias for chunk IDs, which are four bytes meant to be understood as characters.
type ChunkID = [u8; 4];

const ID_FORM: ChunkID = *b"FORM";
const ID_COMMON: ChunkID = *b"COMM";
const ID_SOUND_DATA: ChunkID = *b"SSND";
const ID_MARKER: ChunkID = *b"MARK";
const ID_INSTRUMENT: ChunkID = *b"INST";
const ID_MIDI_DATA: ChunkID = *b"MIDI";
const ID_AUDIO_RECORDING: ChunkID = *b"AESD";
const ID_APPLICATION_SPECIFIC: ChunkID = *b"APPL";
const ID_COMMENTS: ChunkID = *b"COMT";
const ID_NAME: ChunkID = *b"NAME";
const ID_AUTHOR: ChunkID = *b"AUTH";
const ID_COPYRIGHT: ChunkID = *b"(c) ";
const ID_ANNOTATION: ChunkID = *b"ANNO";
const ID_ID3V2: ChunkID = *b"ID3 ";

impl TypelessChunk {
    /// Gives a [`Chunk`] reference from a slice of bytes. This function does check to make sure all
    /// memory that the [`Chunk`] will have within itself and its `data` field are within the slice.
    ///
    /// [`Chunk`]: Chunk
    fn from_bytes(bytes: &[u8]) -> Option<(TypelessChunk, &[u8])> {
        if bytes.len() < 8 {
            return None;
        }

        let id: [u8; 4] = *bytes[0..4].as_array()?;
        let size = i32::from_be_bytes(*bytes[4..8].as_array()?);

        Some((
            TypelessChunk {
                id,
                data: bytes[8..8 + size as usize].to_vec(),
            },
            &bytes[8 + size as usize..],
        ))
    }

    /// Turn the [`TypelessChunk`] back into the bytes which made it up in the original AIFF file.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn to_bytes(mut self) -> Vec<u8> {
        let chunk_size = self.data.len() as u32;

        let size_0 = chunk_size & 0b_00000000_00000000_00000000_01111111;
        let size_1 = chunk_size & 0b_00000000_00000000_00111111_10000000;
        let size_2 = chunk_size & 0b_00000000_00011111_11000000_00000000;
        let size_3 = chunk_size & 0b_00001111_11100000_00000000_00000000;

        let chunk_size = (size_3 << 3) | (size_2 << 2) | (size_1 << 1) | size_0;

        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.id.to_vec());
        bytes.append(&mut chunk_size.to_be_bytes().to_vec());
        bytes.append(&mut self.data);
        bytes
    }

    /// Type this [`TypelessChunk`] by parsing its `id` field.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn typed(self) -> Option<Chunk> {
        match self.id {
            ID_FORM => Some(Chunk::Form(self.form_chunk()?)),
            ID_COMMON => Some(Chunk::Common(self.common_chunk()?)),
            ID_SOUND_DATA => Some(Chunk::SoundData(self.sound_data_chunk()?)),
            ID_MARKER => Some(Chunk::Marker(self.marker_chunk()?)),
            ID_INSTRUMENT => Some(Chunk::Instrument(self.instrument_chunk()?)),
            ID_MIDI_DATA => Some(Chunk::MidiData(self.midi_data_chunk()?)),
            ID_AUDIO_RECORDING => Some(Chunk::AudioRecording(self.audio_recording_chunk()?)),
            ID_APPLICATION_SPECIFIC => Some(Chunk::ApplicationSpecific(
                self.application_specific_chunk()?,
            )),
            ID_COMMENTS => Some(Chunk::Comments(self.comments_chunk()?)),
            ID_NAME => Some(Chunk::Name(self.name_chunk()?)),
            ID_AUTHOR => Some(Chunk::Author(self.author_chunk()?)),
            ID_COPYRIGHT => Some(Chunk::Copyright(self.copyright_chunk()?)),
            ID_ANNOTATION => Some(Chunk::Annotation(self.annotation_chunk()?)),
            ID_ID3V2 => Some(Chunk::Id3v2(self.id3v2_chunk()?)),
            _ => Some(Chunk::Typeless(self)),
        }
    }

    /// Attempt to make a [`FormChunk`] out of this [`TypelessChunk`].
    ///
    /// [`FormChunk`]: FormChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn form_chunk(self) -> Option<FormChunk> {
        if self.data.len() < 4 {
            return None;
        }

        match self.id {
            ID_FORM => Some(FormChunk {
                form_type: *self.data[0..4].as_array()?,
                chunks: parse_chunks(&self.data[4..]).collect(),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`CommonChunk`] out of this [`TypelessChunk`].
    ///
    /// [`CommonChunk`]: CommonChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn common_chunk(self) -> Option<CommonChunk> {
        if self.data.len() != 18 {
            return None;
        }

        match self.id {
            ID_COMMON => Some(CommonChunk {
                channels: i16::from_be_bytes(*self.data[0..2].as_array()?),
                num_sample_frames: u32::from_be_bytes(*self.data[2..6].as_array()?),
                sample_size: i16::from_be_bytes(*self.data[6..8].as_array()?),
                sample_rate: Extended::from_be_bytes(*self.data[8..self.data.len()].as_array()?),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`SoundDataChunk`] out of this [`TypelessChunk`].
    ///
    /// [`SoundDataChunk`]: SoundDataChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn sound_data_chunk(self) -> Option<SoundDataChunk> {
        if self.data.len() < 8 {
            return None;
        }

        match self.id {
            ID_SOUND_DATA => Some(SoundDataChunk {
                offset: u32::from_be_bytes(*self.data[0..4].as_array()?),
                block_size: u32::from_be_bytes(*self.data[4..8].as_array()?),
                sound_data: self.data[8..self.data.len()].to_vec(),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`MarkerChunk`] out of this [`TypelessChunk`].
    ///
    /// [`MarkerChunk`]: MarkerChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn marker_chunk(self) -> Option<MarkerChunk> {
        if self.data.len() < 2 {
            return None;
        }

        match self.id {
            ID_MARKER => Some(MarkerChunk {
                num_markers: u16::from_be_bytes(*self.data[0..2].as_array()?),
                markers: self.data[2..].to_vec(),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`InstrumentChunk`] out of this [`TypelessChunk`].
    ///
    /// [`InstrumentChunk`]: InstrumentChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn instrument_chunk(self) -> Option<InstrumentChunk> {
        if self.data.len() < 20 {
            return None;
        }

        match self.id {
            ID_INSTRUMENT => Some(InstrumentChunk {
                base_note: *self.data.get(0)?,
                detune: *self.data.get(1)?,
                low_note: *self.data.get(2)?,
                high_note: *self.data.get(3)?,
                low_velocity: *self.data.get(4)?,
                high_velocity: *self.data.get(5)?,
                gain: i16::from_be_bytes(*self.data[6..8].as_array()?),
                sustain_loop: Loop {
                    play_mode: LoopPlayMode::from_i16(i16::from_be_bytes(
                        *self.data[8..10].as_array()?,
                    ))?,
                    begin_loop: i16::from_be_bytes(*self.data[10..12].as_array()?),
                    end_loop: i16::from_be_bytes(*self.data[12..14].as_array()?),
                },
                release_loop: Loop {
                    play_mode: LoopPlayMode::from_i16(i16::from_be_bytes(
                        *self.data[14..16].as_array()?,
                    ))?,
                    begin_loop: i16::from_be_bytes(*self.data[16..18].as_array()?),
                    end_loop: i16::from_be_bytes(*self.data[18..20].as_array()?),
                },
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`MidiDataChunk`] out of this [`TypelessChunk`].
    ///
    /// [`MidiDataChunk`]: MidiDataChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn midi_data_chunk(self) -> Option<MidiDataChunk> {
        match self.id {
            ID_MIDI_DATA => Some(MidiDataChunk {
                midi_data: self.data,
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`AudioRecordingChunk`] out of this [`TypelessChunk`].
    ///
    /// [`AudioRecordingChunk`]: AudioRecordingChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn audio_recording_chunk(self) -> Option<AudioRecordingChunk> {
        match self.id {
            ID_AUDIO_RECORDING => Some(AudioRecordingChunk {
                aes_channel_status_data: *self.data.as_array()?,
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`ApplicationSpecificChunk`] out of this [`TypelessChunk`].
    ///
    /// [`ApplicationSpecificChunk`]: ApplicationSpecificChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn application_specific_chunk(self) -> Option<ApplicationSpecificChunk> {
        if self.data.len() < 4 {
            return None;
        }

        match self.id {
            ID_APPLICATION_SPECIFIC => Some(ApplicationSpecificChunk {
                application_signature: *self.data[0..4].as_array()?,
                data: self.data[4..].to_vec(),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`CommentsChunk`] out of this [`TypelessChunk`].
    ///
    /// [`CommentsChunk`]: CommentsChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn comments_chunk(self) -> Option<CommentsChunk> {
        if self.data.len() < 2 {
            return None;
        }

        match self.id {
            ID_COMMENTS => Some(CommentsChunk {
                num_comments: u16::from_be_bytes(*self.data[0..2].as_array()?),
                comments: self.data[2..].to_vec(),
            }),

            _ => None,
        }
    }

    /// Attempt to make a [`NameChunk`] out of this [`TypelessChunk`].
    ///
    /// [`NameChunk`]: NameChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn name_chunk(self) -> Option<NameChunk> {
        match self.id {
            ID_NAME => Some(NameChunk { text: self.data }),
            _ => None,
        }
    }

    /// Attempt to make a [`AuthorChunk`] out of this [`TypelessChunk`].
    ///
    /// [`AuthorChunk`]: AuthorChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn author_chunk(self) -> Option<AuthorChunk> {
        match self.id {
            ID_AUTHOR => Some(AuthorChunk { text: self.data }),
            _ => None,
        }
    }

    /// Attempt to make a [`CopyrightChunk`] out of this [`TypelessChunk`].
    ///
    /// [`CopyrightChunk`]: CopyrightChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn copyright_chunk(self) -> Option<CopyrightChunk> {
        match self.id {
            ID_COPYRIGHT => Some(CopyrightChunk { text: self.data }),
            _ => None,
        }
    }

    /// Attempt to make a [`AnnotationChunk`] out of this [`TypelessChunk`].
    ///
    /// [`AnnotationChunk`]: AnnotationChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn annotation_chunk(self) -> Option<AnnotationChunk> {
        match self.id {
            ID_ANNOTATION => Some(AnnotationChunk { text: self.data }),
            _ => None,
        }
    }

    /// Attempt to make a [`Id3v2Chunk`] out of this [`TypelessChunk`].
    ///
    /// [`Id3v2Chunk`]: Id3v2Chunk
    /// [`TypelessChunk`]: TypelessChunk
    fn id3v2_chunk(self) -> Option<Id3v2Chunk> {
        match self.id {
            ID_ID3V2 => {
                let (tag, _) = id3v2::parse_tag(&self.data).ok()?;
                Some(Id3v2Chunk { tag })
            }
            _ => None,
        }
    }
}

impl Loop {
    /// Get the bytes of the given [`Loop`].
    ///
    /// [`Loop`]: Loop
    fn to_bytes(self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.play_mode.to_i16().to_be_bytes().to_vec());
        bytes.append(&mut self.begin_loop.to_be_bytes().to_vec());
        bytes.append(&mut self.end_loop.to_be_bytes().to_vec());
        bytes
    }
}

impl LoopPlayMode {
    /// Turns the [`LoopPlayMode`] back into its original [`i16`] representation from within the IFF
    /// file.
    ///
    /// [`LoopPlayMode`]: LoopPlayMode
    /// [`i16`]: i16
    fn to_i16(self) -> i16 {
        match self {
            LoopPlayMode::NoLooping => 0,
            LoopPlayMode::ForwardLooping => 1,
            LoopPlayMode::ForwardBackwardLooping => 2,
        }
    }

    /// Creates a [`LoopPlayMode`] from an [`i16`] value, as it should be understood within an AIFF.
    ///
    /// [`LoopPlayMode`]: LoopPlayMode
    /// [`i16`]: i16
    fn from_i16(i: i16) -> Option<LoopPlayMode> {
        match i {
            0 => Some(LoopPlayMode::NoLooping),
            1 => Some(LoopPlayMode::ForwardLooping),
            2 => Some(LoopPlayMode::ForwardBackwardLooping),
            _ => None,
        }
    }
}

/// Check that a given FORM AIFF is valid. Checks the given [`FormChunk`] for basic properties
/// involving the content and presence of its contained [`Chunk`].
///
/// [`FormChunk`]: FormChunk
/// [`Chunk`]: Chunk
fn validate_form_aiff(form: &FormChunk) -> Result<()> {
    // The only FORM chunk should be the one given.
    if form.chunks.iter().any(|c| match c {
        Chunk::Form(_) => true,
        _ => false,
    }) {
        return Err(ParseError::UnexpectedFormChunk);
    }

    // Must have exactly one common chunk.
    match form
        .chunks
        .iter()
        .filter(|c| match c {
            Chunk::Common(_) => true,
            _ => false,
        })
        .count()
    {
        0 => return Err(ParseError::ExpectedCommonChunk),
        1 => (),
        2.. => return Err(ParseError::TooManyCommonChunks),
    };

    // FORM AIFFs must contain at exactly one sound data chunk.
    match form
        .chunks
        .iter()
        .filter(|c| match c {
            Chunk::SoundData(_) => true,
            _ => false,
        })
        .count()
    {
        0 => {
            let num_samples = form.chunks.iter().find_map(|c| match c {
                Chunk::Common(CommonChunk {
                    num_sample_frames, ..
                }) => Some(*num_sample_frames),
                _ => None,
            });

            // If there are no samples then we don't need the sound data chunk.
            if Some(0) != num_samples {
                return Err(ParseError::ExpectedSoundChunk);
            }
        }
        1 => (),
        2.. => return Err(ParseError::TooManySoundChunks),
    };

    // While not part of the AIFF 1.3 documentation, we have no way of deciding between two ID3v2
    // tags, so we restrict it to just one.
    if form
        .chunks
        .iter()
        .filter(|c| match c {
            Chunk::Id3v2(_) => true,
            _ => false,
        })
        .count()
        > 1
    {
        return Err(ParseError::TooManyId3Chunks);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use core::panic;

    use super::TypelessChunk;
    use crate::aiff;
    use extended::Extended;

    #[test]
    #[rustfmt::skip]
    fn chunk_to_bytes() {
        let file = aiff::File {
            form_aiff: aiff::FormChunk {
                form_type: *b"AIFF",
                chunks: vec![aiff::Chunk::Common(aiff::CommonChunk {
                    channels: 2,
                    num_sample_frames: 0,
                    sample_size: 16,
                    sample_rate: Extended::from(48_000_f64),
                })],
            },
        };

        let bytes = aiff::Chunk::Form(file.form_aiff).untype().to_bytes();
        let expected_size = /* AIFF */ 4 + /* COMM */ 4 + /* common size */ 4 + /* common chunk data */ 18;

        assert_eq!(
            bytes[0..28],
            [
                // FORM ID
                b'F', b'O', b'R', b'M',

                // Total size, or size of FORM
                0, 0, 0, expected_size,

                // File type tag "AIFF"
                b'A', b'I', b'F', b'F',

                // Common chunk tag
                b'C', b'O', b'M', b'M',

                // common chunk size
                0, 0, 0, 18,

                // channels
                0, 2,

                // number of samples
                0, 0, 0, 0,

                // sample size
                0, 16,
            ]
        )
    }

    #[test]
    fn chunk_from_bytes() {
        let bytes = [
            b'F', b'O', b'R', b'M', 0, 0, 0, 4, b'A', b'I', b'F', b'F', 1, 2, 3, 4,
        ];

        let (chunk, remainder) = TypelessChunk::from_bytes(&bytes).unwrap();

        assert_eq!(&chunk.id, b"FORM");
        assert_eq!(chunk.data.len(), 4);
        assert_eq!(&chunk.data, b"AIFF");

        assert_eq!(remainder, &[1, 2, 3, 4]);
    }

    #[test]
    fn form_chunk() {
        let bytes = [b'F', b'O', b'R', b'M', 0, 0, 0, 4, b'A', b'I', b'F', b'F'];
        let (chunk, _) = TypelessChunk::from_bytes(&bytes).unwrap();
        let typed_chunk = chunk.typed().unwrap();

        match typed_chunk {
            aiff::Chunk::Form(form_chunk) => {
                assert_eq!(form_chunk.form_type, *b"AIFF");
                assert_eq!(form_chunk.chunks.len(), 0);
            }

            _ => panic!("Expected FORM chunk"),
        }
    }

    #[test]
    #[rustfmt::skip]
    fn common_chunk() {
        let sample_rate = Extended::from(44_000_f64);
        let mut bytes = vec![
            // ID
            b'C', b'O', b'M', b'M',

            // Size
            0, 0, 0, 18,

            // Channels
            0, 2,

            // Number of samples
            255, 255, 255, 255,

            // Sample size
            0, 32,
        ];
        bytes.append(&mut sample_rate.to_be_bytes().to_vec());

        let (chunk, _) = TypelessChunk::from_bytes(&bytes).unwrap();
        let typed_chunk= chunk.typed().unwrap();

        match typed_chunk{
            aiff::Chunk::Common(common_chunk) => {
                assert_eq!(common_chunk.channels, 2);
                assert_eq!(common_chunk.num_sample_frames, u32::MAX);
                assert_eq!(common_chunk.sample_size, 32);
                assert_eq!(common_chunk.sample_rate, sample_rate);
                assert_eq!(common_chunk.sample_rate.to_f64(), 44_000_f64);
            },

            _ => panic!("Expected chunk to be a common chunk"),
        }
    }
}
