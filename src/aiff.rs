use crate::id3;
use crate::tags::{Audio, AudioTag, AudioTagged};
use extended::Extended;
use std::path::Path;
use std::{fs, io, result};

/// An AIFF file (FORM AIFF).
#[derive(Debug, Clone)]
pub struct File {
    chunks: Vec<Chunk>,
}

impl File {
    pub fn open(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|e| ParseError::Io(e))?;
        let top_level_chunks: Vec<Chunk> = parse_chunks(&bytes).collect();

        if top_level_chunks.len() > 1 {
            return Err(ParseError::ExcessTopLevelChunks);
        }

        match top_level_chunks.first() {
            Some(Chunk::Form(form_chunk)) => {
                let chunks: Vec<Chunk> = parse_chunks(&form_chunk.chunks).collect();

                if chunks.iter().any(|c| match c {
                    Chunk::Form(_) => true,
                    _ => false,
                }) {
                    return Err(ParseError::UnexpectedFormChunk);
                }

                if chunks
                    .iter()
                    .filter(|c| match c {
                        Chunk::SoundData(_) => true,
                        _ => false,
                    })
                    .count()
                    > 1
                {
                    return Err(ParseError::TooManySoundChunks);
                }

                if chunks
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

                match chunks
                    .iter()
                    .filter(|c| match c {
                        Chunk::Common(_) => true,
                        _ => false,
                    })
                    .count()
                {
                    0 => Err(ParseError::ExpectedCommonChunk),
                    1 => Ok(File { chunks }),
                    2.. => Err(ParseError::TooManyCommonChunks),
                }
            }

            _ => Err(ParseError::ExpectedFormChunk),
        }
    }
}

impl Audio for File {
    fn sample_rate(&self) -> f64 {
        // When an `aiff::File` is made we check for the common chunk, so this should never panic.
        self.chunks
            .iter()
            .find_map(|c| match c {
                Chunk::Common(CommonChunk { sample_rate, .. }) => Some(sample_rate.to_f64()),
                _ => None,
            })
            .expect("Common chunk should always exist.")
    }

    fn sample_size(&self) -> u16 {
        self.chunks
            .iter()
            .find_map(|c| match c {
                Chunk::Common(CommonChunk { sample_size, .. }) => Some(*sample_size as _),
                _ => None,
            })
            .expect("Common chunk should always exist.")
    }

    fn channels(&self) -> u16 {
        self.chunks
            .iter()
            .find_map(|c| match c {
                Chunk::Common(CommonChunk { channels, .. }) => Some(*channels as _),
                _ => None,
            })
            .expect("Common chunk should always exist.")
    }
}

impl AudioTagged for File {
    fn get_tag(&self, audio_tag: AudioTag) -> Option<String> {
        self.chunks.iter().find_map(|c| match c {
            Chunk::Id3v2(Id3v2Chunk { tag }) => tag.get_tag(audio_tag),
            _ => None,
        })
    }
}

pub type Result<T> = result::Result<T, ParseError>;

pub enum ParseError {
    Io(io::Error),
    ExcessTopLevelChunks,
    ExpectedFormChunk,
    UnexpectedFormChunk,
    ExpectedCommonChunk,
    TooManyCommonChunks,
    TooManySoundChunks,
    TooManyId3Chunks,
}

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

#[derive(Debug, Clone)]
enum Chunk {
    Typeless(TypelessChunk),
    Form(FormChunk),
    Common(CommonChunk),
    SoundData(SoundDataChunk),
    Marker(MarkerChunk),
    Instrument(InstrumentChunk),
    MidiData(MidiDataChunk),
    AudioRecording(AudioRecordingChunk),
    ApplicationSpecific(ApplicationSpecificChunk),
    Comments(CommentsChunk),
    Name(NameChunk),
    Author(AuthorChunk),
    Copyright(CopyrightChunk),
    Annotation(AnnotationChunk),
    Id3v2(Id3v2Chunk),
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

    /// The remaining `data` from the original [`Chunk`]. For a FORM chunk this should contains
    /// other chunks of the file.
    ///
    /// [`Chunk`]: Chunk
    chunks: Vec<u8>,
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
    tag: id3::Tag,
}

/// Type alias for marker IDs.
type MarkerId = i16;

/// Type alias for the `OSType` defined in _Inside Macintosh, vol II_, which is used by AIFF.
type OsType = [u8; 4];

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

#[repr(i16)]
#[derive(Debug, Clone, Copy)]
enum LoopPlayMode {
    NoLooping = 0,
    ForwardLooping = 1,
    ForwardBackwardLooping = 2,
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

impl TypelessChunk {
    /// Gives a [`Chunk`] reference from a slice of bytes. This function does check to make sure all
    /// memory that the [`Chunk`] will have within itself and its `data` field are within the slice.
    ///
    /// [`Chunk`]: Chunk
    fn from_bytes(bytes: &[u8]) -> Option<(TypelessChunk, &[u8])> {
        if bytes.len() < size_of::<[u8; 4]>() + size_of::<i32>() {
            return None;
        }

        let id: [u8; 4] = *bytes[0..size_of::<[u8; 4]>()].as_array()?;
        let size = i32::from_be_bytes(
            *bytes[size_of::<[u8; 4]>()..size_of::<[u8; 4]>() + size_of::<i32>()].as_array()?,
        );
        let head_size = size_of_val(&id) + size_of_val(&size);

        Some((
            TypelessChunk {
                id,
                data: bytes[head_size..head_size + size as usize].to_vec(),
            },
            &bytes[head_size + size as usize..],
        ))
    }

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

    fn form_chunk(self) -> Option<FormChunk> {
        match self.id {
            ID_FORM => Some(FormChunk {
                form_type: *self.data[0..size_of::<[u8; 4]>()].as_array()?,
                chunks: self.data[size_of::<[u8; 4]>()..].to_vec(),
            }),

            _ => None,
        }
    }

    fn common_chunk(self) -> Option<CommonChunk> {
        match self.id {
            ID_COMMON => Some(CommonChunk {
                // this looks confusing, but its just taking the size of all previous fields summed
                // to that plus the size of the next field as a new slice, and using it to form a
                // field of the struct.
                channels: i16::from_be_bytes(*self.data[0..size_of::<i16>()].as_array()?),

                num_sample_frames: u32::from_be_bytes(
                    *self.data[size_of::<i16>()..size_of::<i16>() + size_of::<u32>()].as_array()?,
                ),

                sample_size: i16::from_be_bytes(
                    *self.data[size_of::<i16>() + size_of::<u32>()
                        ..size_of::<i16>() + size_of::<u32>() + size_of::<i16>()]
                        .as_array()?,
                ),

                sample_rate: Extended::from_be_bytes(
                    *self.data
                        [size_of::<i16>() + size_of::<u32>() + size_of::<i16>()..self.data.len()]
                        .as_array()?,
                ),
            }),

            _ => None,
        }
    }

    fn sound_data_chunk(self) -> Option<SoundDataChunk> {
        match self.id {
            ID_SOUND_DATA => Some(SoundDataChunk {
                offset: u32::from_be_bytes(*self.data[0..size_of::<u32>()].as_array()?),
                block_size: u32::from_be_bytes(
                    *self.data[size_of::<u32>()..size_of::<u32>() + size_of::<u32>()].as_array()?,
                ),
                sound_data: self.data[size_of::<u32>() + size_of::<u32>()..self.data.len()]
                    .to_vec(),
            }),

            _ => None,
        }
    }

    fn marker_chunk(self) -> Option<MarkerChunk> {
        match self.id {
            ID_MARKER => Some(MarkerChunk {
                num_markers: u16::from_be_bytes(*self.data[0..size_of::<u16>()].as_array()?),
                markers: self.data[size_of::<u16>()..].to_vec(),
            }),

            _ => None,
        }
    }

    fn instrument_chunk(self) -> Option<InstrumentChunk> {
        match self.id {
            ID_INSTRUMENT => Some(InstrumentChunk {
                base_note: *self.data.get(0)?,
                detune: *self.data.get(1)?,
                low_note: *self.data.get(3)?,
                high_note: *self.data.get(4)?,
                low_velocity: *self.data.get(5)?,
                high_velocity: *self.data.get(6)?,
                gain: i16::from_be_bytes(*self.data[7..7 + size_of::<i16>()].as_array()?),
                sustain_loop: Loop {
                    play_mode: LoopPlayMode::from_i16(i16::from_be_bytes(
                        *self.data[7 + size_of::<i16>()..7 + 2 * size_of::<i16>()].as_array()?,
                    ))?,
                    begin_loop: i16::from_be_bytes(
                        *self.data[7 + 2 * size_of::<i16>()..7 + 3 * size_of::<i16>()]
                            .as_array()?,
                    ),
                    end_loop: i16::from_be_bytes(
                        *self.data[7 + 3 * size_of::<i16>()..7 + 4 * size_of::<i16>()]
                            .as_array()?,
                    ),
                },
                release_loop: Loop {
                    play_mode: LoopPlayMode::from_i16(i16::from_be_bytes(
                        *self.data[7 + 4 * size_of::<i16>()..7 + 5 * size_of::<i16>()]
                            .as_array()?,
                    ))?,
                    begin_loop: i16::from_be_bytes(
                        *self.data[7 + 5 * size_of::<i16>()..7 + 6 * size_of::<i16>()]
                            .as_array()?,
                    ),
                    end_loop: i16::from_be_bytes(
                        *self.data[7 + 6 * size_of::<i16>()..7 + 7 * size_of::<i16>()]
                            .as_array()?,
                    ),
                },
            }),

            _ => None,
        }
    }

    fn midi_data_chunk(self) -> Option<MidiDataChunk> {
        match self.id {
            ID_MIDI_DATA => Some(MidiDataChunk {
                midi_data: self.data,
            }),

            _ => None,
        }
    }

    fn audio_recording_chunk(self) -> Option<AudioRecordingChunk> {
        match self.id {
            ID_AUDIO_RECORDING => Some(AudioRecordingChunk {
                aes_channel_status_data: *self.data.as_array()?,
            }),

            _ => None,
        }
    }

    fn application_specific_chunk(self) -> Option<ApplicationSpecificChunk> {
        match self.id {
            ID_APPLICATION_SPECIFIC => Some(ApplicationSpecificChunk {
                application_signature: *self.data[0..size_of::<[u8; 4]>()].as_array()?,
                data: self.data[size_of::<[u8; 4]>()..].to_vec(),
            }),

            _ => None,
        }
    }

    fn comments_chunk(self) -> Option<CommentsChunk> {
        match self.id {
            ID_COMMENTS => Some(CommentsChunk {
                num_comments: u16::from_be_bytes(*self.data[0..size_of::<u16>()].as_array()?),
                comments: self.data[size_of::<i16>()..].to_vec(),
            }),

            _ => None,
        }
    }

    fn name_chunk(self) -> Option<NameChunk> {
        match self.id {
            ID_NAME => Some(NameChunk { text: self.data }),
            _ => None,
        }
    }

    fn author_chunk(self) -> Option<AuthorChunk> {
        match self.id {
            ID_AUTHOR => Some(AuthorChunk { text: self.data }),
            _ => None,
        }
    }

    fn copyright_chunk(self) -> Option<CopyrightChunk> {
        match self.id {
            ID_COPYRIGHT => Some(CopyrightChunk { text: self.data }),
            _ => None,
        }
    }

    fn annotation_chunk(self) -> Option<AnnotationChunk> {
        match self.id {
            ID_ANNOTATION => Some(AnnotationChunk { text: self.data }),
            _ => None,
        }
    }

    fn id3v2_chunk(self) -> Option<Id3v2Chunk> {
        match self.id {
            ID_ID3V2 => {
                let tag = id3::parse_tag(&self.data)?;
                Some(Id3v2Chunk { tag })
            }
            _ => None,
        }
    }
}

impl LoopPlayMode {
    fn from_i16(i: i16) -> Option<LoopPlayMode> {
        match i {
            0 => Some(LoopPlayMode::NoLooping),
            1 => Some(LoopPlayMode::ForwardLooping),
            2 => Some(LoopPlayMode::ForwardBackwardLooping),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Chunk, TypelessChunk};
    use crate::{
        aiff::parse_chunks,
        id3,
        tags::{self, AudioTagged},
    };
    use core::panic;
    use std::{ascii, fs};

    #[test]
    fn chunk_from_bytes() {
        let bytes = [
            b'F', b'O', b'R', b'M', 0, 0, 0, 4, b'A', b'I', b'F', b'F', 1, 2, 3, 4,
        ];

        let (chunk, remainder) = TypelessChunk::from_bytes(&bytes).unwrap();

        assert_eq!(&chunk.id, b"FORM");
        assert_eq!(chunk.data.len(), 4);
        assert_eq!(chunk.data, b"AIFF" as &[u8]);

        assert_eq!(remainder, &[1, 2, 3, 4]);
    }

    #[test]
    fn form_chunk() {
        let bytes = [b'F', b'O', b'R', b'M', 0, 0, 0, 4, b'A', b'I', b'F', b'F'];
        let (chunk, _) = TypelessChunk::from_bytes(&bytes).unwrap();

        assert_eq!(chunk.form_chunk().unwrap().chunks.len(), 0);
    }

    #[test]
    fn common_chunk() {
        let bytes = [
            b'C', b'O', b'M', b'M', 0, 0, 0, 18, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
            16, 17, 18,
        ];
        let (chunk, _) = TypelessChunk::from_bytes(&bytes).unwrap();

        assert!(chunk.common_chunk().is_some());
    }
}
