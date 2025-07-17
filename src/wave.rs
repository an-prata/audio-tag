// NOTE: Waveform files are appearantly little endian :(

use crate::audio_info::{self, Audio, ReadTag, TrackNumber, WriteTag, WriteTagError};
use std::{error::Error, fmt::Display, fs, io, path::Path, result};

type ChunkId = [u8; 4];

const ID_RIFF: ChunkId = *b"RIFF";
const ID_PAD: ChunkId = *b"PAD ";
const ID_JUNK: ChunkId = *b"JUNK";
const ID_FACT: ChunkId = *b"fact";
const ID_WAVE_FORMAT: ChunkId = *b"fmt ";
const ID_WAVE_DATA: ChunkId = *b"data";
const ID_LIST: ChunkId = *b"LIST";

// Text chunk IDs
const ID_ARCHIVAL_LOCATION: ChunkId = *b"IARL";
const ID_ARTIST: ChunkId = *b"IART";
const ID_COMMISSIONED: ChunkId = *b"ICMS";
const ID_COMMENTS: ChunkId = *b"ICMT";
const ID_COPYRIGHT: ChunkId = *b"ICOP";
const ID_CREATION_DATE: ChunkId = *b"ICRD";
const ID_CROPPED: ChunkId = *b"ICRP";
const ID_DIMENSIONS: ChunkId = *b"IDIM";
const ID_DOTS_PER_INCH: ChunkId = *b"IDPI";
const ID_ENGINEER: ChunkId = *b"IENG";
const ID_GENRE: ChunkId = *b"IGNR";
const ID_KEYWORDS: ChunkId = *b"IKEY";
const ID_LIGHTNESS_SETTINGS: ChunkId = *b"ILGT";
const ID_MEDIUM: ChunkId = *b"IMED";
const ID_NAME: ChunkId = *b"INAM";
const ID_PALETTE_SETTINGS: ChunkId = *b"IPLT";
const ID_PRODUCT: ChunkId = *b"IPRD";
const ID_DESCRIPTION: ChunkId = *b"ISBJ";
const ID_SOFTWARE: ChunkId = *b"ISFT";
const ID_SHARPNESS: ChunkId = *b"ISHP";
const ID_SOURCE: ChunkId = *b"ISRC";
const ID_SOURCE_FORM: ChunkId = *b"ISRF";
const ID_TECHNICIAN: ChunkId = *b"ITCH";
const ID_SMPTE_TIME_CODE: ChunkId = *b"ISMP";
const ID_DIGITIZATION_TIME: ChunkId = *b"IDIT";
const ID_TRACK_NUMBER: ChunkId = *b"ITRK";
const ID_TABLE_OF_CONTENTS: ChunkId = *b"ITOC";

/// A RIFF file, which here we assume is a WAVE file, but it technically doesn't have to be. In the
/// case that a non-WAVE RIFF file is parsed, its audio-related functions will fail as they would if
/// the file is malformed.
#[derive(Debug, Clone)]
pub struct File {
    /// The top level [`RiffChunk`] containing all this RIFF's other [`Chunk`]s.
    ///
    /// [`RiffChunk`]: RiffChunk
    /// [`Chunk`]: Chunk
    riff: RiffChunk,
}

audio_info::impl_read_tag!(File, File::info_list);
audio_info::impl_write_tag!(File, File::info_list_mut);

impl File {
    /// Read a new [`wave::File`] from a file at the given [`AsRef<Path>`].
    ///
    /// [`wave::File`]: File
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn read_from(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|io_err| ParseError::Io(io_err))?;
        File::from_bytes(&bytes)
    }

    /// Write [`wave::File`] to the given [`AsRef<Path>`].
    ///
    /// [`wave::File`]: File
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn write_to(self, path: impl AsRef<Path>) -> Result<()> {
        let bytes = self.to_bytes()?;
        fs::write(path, bytes).map_err(|io_err| ParseError::Io(io_err))
    }

    /// Parse a [`wave::File`] from bytes.
    ///
    /// [`wave::File`]: File
    fn from_bytes(bytes: &[u8]) -> Result<File> {
        let mut top_level_chunks: Vec<Chunk> = ChunksIter {
            remaining_bytes: bytes,
        }
        .collect();

        if top_level_chunks.len() > 1 {
            return Err(ParseError::ExcessTopLevelChunks);
        }

        if let Some(Chunk::Riff(riff)) = top_level_chunks.pop() {
            riff.check()?;
            Ok(File { riff })
        } else {
            return Err(ParseError::ExpectedRiffChunk);
        }
    }

    /// Turn this [`wave::File`] into bytes which represent a Waveform file.
    ///
    /// [`wave::File`]: File
    fn to_bytes(self) -> Result<Vec<u8>> {
        Chunk::Riff(self.riff).to_bytes()
    }

    /// Get the contained [`InfoListChunk`] if it exists.
    ///
    /// [`InfoListChunk`]: InfoListChunk
    fn info_list(&self) -> Option<&InfoListChunk> {
        self.riff.chunks.iter().find_map(|chunk| match chunk {
            Chunk::InfoList(info_list) => Some(info_list),
            _ => None,
        })
    }

    /// Get a mutable reference to the contained [`InfoListChunk`] if it exists.
    ///
    /// [`InfoListChunk`]: InfoListChunk
    fn info_list_mut(&mut self) -> Option<&mut InfoListChunk> {
        self.riff.chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::InfoList(info_list) => Some(info_list),
            _ => None,
        })
    }
}

impl Audio for File {
    fn sample_rate(&self) -> f64 {
        self.riff
            .chunks
            .iter()
            .find_map(|chunk| match chunk {
                Chunk::WaveFormat(WaveFormatChunk {
                    samples_per_second, ..
                }) => Some(*samples_per_second as f64),
                _ => None,
            })
            .unwrap_or(0f64)
    }

    fn sample_size(&self) -> u16 {
        self.riff
            .chunks
            .iter()
            .find_map(|chunk| match chunk {
                Chunk::WaveFormat(WaveFormatChunk {
                    bits_per_sample, ..
                }) => Some(*bits_per_sample),
                _ => None,
            })
            .unwrap_or(0)
    }

    fn channels(&self) -> u16 {
        self.riff
            .chunks
            .iter()
            .find_map(|chunk| match chunk {
                Chunk::WaveFormat(WaveFormatChunk { channels, .. }) => Some(*channels),
                _ => None,
            })
            .unwrap_or(0)
    }
}

/// A wrapping enum around all possible chunk types that may occur in a [`wave::File`].
///
/// [`wave::File`]: File
#[derive(PartialEq, Eq, Clone, Debug)]
enum Chunk {
    Typeless(TypelessChunk),
    Riff(RiffChunk),
    WaveFormat(WaveFormatChunk),
    Fact(FactChunk),
    Junk(JunkChunk),
    Pad(PadChunk),
    WaveData(WaveDataChunk),
    List(ListChunk),
    InfoList(InfoListChunk),

    ArchivalLocation(ArchivalLocationChunk),
    Artist(ArtistChunk),
    Commissioned(CommissionedChunk),
    Comments(CommentsChunk),
    Copyright(CopyrightChunk),
    CreationDate(CreationDateChunk),
    Cropped(CroppedChunk),
    Dimensions(DimensionsChunk),
    DotsPerInch(DotsPerInchChunk),
    Engineer(EngineerChunk),
    Genre(GenreChunk),
    Keywords(KeywordsChunk),
    LightnessSettings(LightnessSettingsChunk),
    Medium(MediumChunk),
    Name(NameChunk),
    PaletteSettings(PaletteSettingsChunk),
    Product(ProductChunk),
    Description(DescriptionChunk),
    Software(SoftwareChunk),
    Sharpness(SharpnessChunk),
    Source(SourceChunk),
    SourceForm(SourceFormChunk),
    Technician(TechnicianChunk),
    SmpteTimeCode(SmpteTimeCodeChunk),
    DigitizationTime(DigitizationTimeChunk),
    TrackNumber(TrackNumberChunk),
    TableOfContents(TableOfContentsChunk),
}

impl Chunk {
    /// Returns this [`Chunk`] to being a [`TypelessChunk`].
    ///
    /// [`Chunk`]: Chunk
    /// [`TypelessChunk`]: TypelessChunk
    fn detype(self) -> Result<TypelessChunk> {
        match self {
            Chunk::Typeless(typeless_chunk) => Ok(typeless_chunk),

            Chunk::Riff(RiffChunk { tag, chunks }) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut tag.to_vec());

                for chunk in chunks {
                    bytes.append(&mut chunk.to_bytes()?);
                }

                Ok(TypelessChunk {
                    id: ID_RIFF,
                    data: bytes,
                })
            }

            Chunk::WaveFormat(WaveFormatChunk {
                format,
                channels,
                samples_per_second,
                average_bytes_per_second,
                block_align,
                bits_per_sample,
            }) => {
                let mut bytes: Vec<u8> = Vec::new();
                let (format_tag, mut extension) = format.to_bytes();
                bytes.append(&mut format_tag.to_le_bytes().to_vec());
                bytes.append(&mut channels.to_le_bytes().to_vec());
                bytes.append(&mut samples_per_second.to_le_bytes().to_vec());
                bytes.append(&mut average_bytes_per_second.to_le_bytes().to_vec());
                bytes.append(&mut block_align.to_le_bytes().to_vec());
                bytes.append(&mut bits_per_sample.to_le_bytes().to_vec());
                bytes.append(&mut extension);
                Ok(TypelessChunk {
                    id: ID_RIFF,
                    data: bytes,
                })
            }
            Chunk::Fact(FactChunk { sample_length }) => Ok(TypelessChunk {
                id: ID_FACT,
                data: sample_length.to_le_bytes().to_vec(),
            }),
            Chunk::Junk(JunkChunk(data)) => Ok(TypelessChunk { id: ID_JUNK, data }),
            Chunk::Pad(PadChunk(data)) => Ok(TypelessChunk { id: ID_PAD, data }),
            Chunk::WaveData(WaveDataChunk { data }) => Ok(TypelessChunk {
                id: ID_WAVE_DATA,
                data,
            }),
            Chunk::List(ListChunk { tag, chunks }) => {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.append(&mut tag.to_vec());

                for chunk in chunks {
                    bytes.append(&mut chunk.to_bytes()?);
                }

                Ok(TypelessChunk {
                    id: ID_RIFF,
                    data: bytes,
                })
            }
            Chunk::InfoList(InfoListChunk(chunks)) => {
                let mut bytes: Vec<u8> = vec![b'I', b'N', b'F', b'O'];

                for chunk in chunks {
                    bytes.append(&mut chunk.to_bytes()?);
                }

                Ok(TypelessChunk {
                    id: ID_RIFF,
                    data: bytes,
                })
            }

            Chunk::ArchivalLocation(ArchivalLocationChunk(text)) => TextChunk {
                id: ID_ARCHIVAL_LOCATION,
                text,
            }
            .detype(),

            Chunk::Artist(ArtistChunk(text)) => TextChunk {
                id: ID_ARTIST,
                text,
            }
            .detype(),

            Chunk::Commissioned(CommissionedChunk(text)) => TextChunk {
                id: ID_COMMISSIONED,
                text,
            }
            .detype(),

            Chunk::Comments(CommentsChunk(text)) => TextChunk {
                id: ID_COMMENTS,
                text,
            }
            .detype(),

            Chunk::Copyright(CopyrightChunk(text)) => TextChunk {
                id: ID_COPYRIGHT,
                text,
            }
            .detype(),

            Chunk::CreationDate(CreationDateChunk(text)) => TextChunk {
                id: ID_CREATION_DATE,
                text,
            }
            .detype(),

            Chunk::Cropped(CroppedChunk(text)) => TextChunk {
                id: ID_CROPPED,
                text,
            }
            .detype(),

            Chunk::Dimensions(DimensionsChunk(text)) => TextChunk {
                id: ID_DIMENSIONS,
                text,
            }
            .detype(),

            Chunk::DotsPerInch(DotsPerInchChunk(text)) => TextChunk {
                id: ID_DOTS_PER_INCH,
                text,
            }
            .detype(),

            Chunk::Engineer(EngineerChunk(text)) => TextChunk {
                id: ID_ENGINEER,
                text,
            }
            .detype(),

            Chunk::Genre(GenreChunk(text)) => TextChunk { id: ID_GENRE, text }.detype(),

            Chunk::Keywords(KeywordsChunk(text)) => TextChunk {
                id: ID_KEYWORDS,
                text,
            }
            .detype(),

            Chunk::LightnessSettings(LightnessSettingsChunk(text)) => TextChunk {
                id: ID_LIGHTNESS_SETTINGS,
                text,
            }
            .detype(),

            Chunk::Medium(MediumChunk(text)) => TextChunk {
                id: ID_MEDIUM,
                text,
            }
            .detype(),

            Chunk::Name(NameChunk(text)) => TextChunk { id: ID_NAME, text }.detype(),

            Chunk::PaletteSettings(PaletteSettingsChunk(text)) => TextChunk {
                id: ID_PALETTE_SETTINGS,
                text,
            }
            .detype(),

            Chunk::Product(ProductChunk(text)) => TextChunk {
                id: ID_PRODUCT,
                text,
            }
            .detype(),

            Chunk::Description(DescriptionChunk(text)) => TextChunk {
                id: ID_DESCRIPTION,
                text,
            }
            .detype(),

            Chunk::Software(SoftwareChunk(text)) => TextChunk {
                id: ID_SOFTWARE,
                text,
            }
            .detype(),

            Chunk::Sharpness(SharpnessChunk(text)) => TextChunk {
                id: ID_SHARPNESS,
                text,
            }
            .detype(),

            Chunk::Source(SourceChunk(text)) => TextChunk {
                id: ID_SOURCE,
                text,
            }
            .detype(),

            Chunk::SourceForm(SourceFormChunk(text)) => TextChunk {
                id: ID_SOURCE_FORM,
                text,
            }
            .detype(),

            Chunk::Technician(TechnicianChunk(text)) => TextChunk {
                id: ID_TECHNICIAN,
                text,
            }
            .detype(),

            Chunk::SmpteTimeCode(SmpteTimeCodeChunk(text)) => TextChunk {
                id: ID_SMPTE_TIME_CODE,
                text,
            }
            .detype(),

            Chunk::DigitizationTime(DigitizationTimeChunk(text)) => TextChunk {
                id: ID_DIGITIZATION_TIME,
                text,
            }
            .detype(),

            Chunk::TrackNumber(TrackNumberChunk(text)) => TextChunk {
                id: ID_TRACK_NUMBER,
                text,
            }
            .detype(),

            Chunk::TableOfContents(TableOfContentsChunk(text)) => TextChunk {
                id: ID_TABLE_OF_CONTENTS,
                text,
            }
            .detype(),
        }
    }

    /// Turn this [`Chunk`] into bytes which can be parsed back into a [`Chunk`].
    ///
    /// [`Chunk`]: Chunk
    fn to_bytes(self) -> Result<Vec<u8>> {
        self.detype().map(TypelessChunk::to_bytes)
    }
}

pub enum WriteError {
    /// A [`String`] which is expected to be an [`AsciiString`] failed the conversion to ASCII.
    ///
    /// [`String`]: String
    /// [`AsciiString`]: AsciiStr
    NonAsciiString,
}

/// [`Result`] with [`ParseError`] prefilled.
///
/// [`Result`]: result::Result
/// [`ParseError`]: ParseError
type Result<T> = result::Result<T, ParseError>;

/// Errors that may occur when parsing [`wave::File`]s.
///
/// [`wave::File`]: File
#[derive(Debug)]
pub enum ParseError {
    /// An [`io::Error`] occured while reading a Waveform file.
    ///
    /// [`io::Error`]: io::Error
    Io(io::Error),

    /// RIFF Waveform files should have just a single 'RIFF' chunk which contains all other chunks,
    /// but more than one chunk was found at this top level.
    ExcessTopLevelChunks,

    /// Expected a [`RiffChunk`] and did not find one.
    ///
    /// [`RiffChunk`]: RiffChunk
    ExpectedRiffChunk,

    /// Expected a [`WaveFormatChunk`] but did not find one.
    ///
    /// [`WaveFormatChunk`]: WaveFormatChunk
    ExpectedFormatChunk,

    /// Expected a [`WaveDataChunk`] chunk but did not find one.
    ///
    /// [`WaveDataChunk`]: WaveDataChunk
    ExpectedWaveDataChunk,

    /// The [`WaveFormatChunk`] did not occure before the [`WaveDataChunk`], which is required by
    /// Waveform files.
    ///
    /// [`WaveDataChunk`]: WaveDataChunk
    /// [`WaveFormatChunk`]: WaveFormatChunk
    FormatNotBeforeData,

    /// Expected a [`FactChunk`] but did not find one.
    ///
    /// [`FactChunk`]: FactChunk
    ExpectedFactChunk,

    /// Expected [`String`] to be ASCII but it was not.
    ///
    /// [`String`]: String
    NonAsciiString,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Io(error) => {
                write!(f, "could not parse wave file due to IO error: {}", error)
            }
            ParseError::ExcessTopLevelChunks => {
                write!(f, "too many top level chunks, expected just one RIFF chunk")
            }
            ParseError::ExpectedRiffChunk => write!(f, "expected a RIFF chunk but none were found"),
            ParseError::ExpectedFormatChunk => {
                write!(f, "expected a format chunk but none were found")
            }
            ParseError::ExpectedWaveDataChunk => {
                write!(f, "expected a wave data chunk but none were found")
            }
            ParseError::FormatNotBeforeData => {
                write!(
                    f,
                    "expected a format chunk before wave data chunk but reverse order was found"
                )
            }
            ParseError::ExpectedFactChunk => {
                write!(
                    f,
                    "WAVE's format requires a FACT chunk, but none were found"
                )
            }
            ParseError::NonAsciiString => {
                write!(f, "expected `String` to be ASCII, but it was not")
            }
        }
    }
}

impl Error for ParseError {}

/// The top most level chunk in a RIFF WAVE.
#[derive(PartialEq, Eq, Clone, Debug)]
struct RiffChunk {
    tag: [u8; 4],
    chunks: Vec<Chunk>,
}

impl RiffChunk {
    /// Check the chunk structure for validity. This will require a [`FactChunk`] for formats which
    /// require it, it checks for a [`WaveFormatChunk`] and [`WaveDataChunk`], as well as the correct
    /// order of the two.
    ///
    /// [`FactChunk`]: FactChunk
    /// [`WaveFormatChunk`]: WaveFormatChunk
    /// [`WaveDataChunk`]: WaveDataChunk
    fn check(&self) -> Result<()> {
        let fmt_position = self
            .chunks
            .iter()
            .position(|c| match c {
                Chunk::WaveFormat(_) => true,
                _ => false,
            })
            .ok_or(ParseError::ExpectedFormatChunk)?;

        let wave_data_position = self
            .chunks
            .iter()
            .position(|c| match c {
                Chunk::WaveData(_) => true,
                _ => false,
            })
            .ok_or(ParseError::ExpectedWaveDataChunk)?;

        if !(fmt_position < wave_data_position) {
            return Err(ParseError::FormatNotBeforeData);
        }

        let format = self.chunks.iter().find_map(|c| match c {
            Chunk::WaveFormat(WaveFormatChunk { format, .. }) => Some(format),
            _ => None,
        });

        if let Some(WaveFormat::MicrosoftPCM) = format {
            return Ok(());
        } else {
            self.chunks
                .iter()
                .find(|c| match c {
                    Chunk::Fact(_) => true,
                    _ => false,
                })
                .ok_or(ParseError::ExpectedFactChunk)?;
            Ok(())
        }
    }
}

/// Holds information common to all WAVE formats, as well as a format dependent extension with
/// information determined by the WAVE file's format.
#[derive(PartialEq, Eq, Clone, Debug)]
struct WaveFormatChunk {
    format: WaveFormat,
    channels: u16,
    samples_per_second: u32,
    average_bytes_per_second: u32,
    block_align: u16,
    bits_per_sample: u16,
}

/// A chunk which contains no relevent data, and may be ignored. Inteded as a way to reserve space.
#[derive(PartialEq, Eq, Clone, Debug)]
struct JunkChunk(Vec<u8>);

/// A chunk which contains no relevent data, and may be ignored. Inteded as a way to reserve space or
/// pad. When duplicating a file [`PadChunk`]s must be preserved, and the alignment they provide
/// should be maintained even if it means resizing the [`PadChunk`].
///
/// [`PadChunk`]: PadChunk
#[derive(PartialEq, Eq, Clone, Debug)]
struct PadChunk(Vec<u8>);

/// Stores file dependent information about the contents of the WAVE file.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
struct FactChunk {
    /// The length of data in individual samples. This can be used in conjuction with
    /// [`WaveFormatChunk`]'s `samples_per_second` field in order to determine the length of the
    /// file's data in seconds.
    ///
    /// [`WaveFormatChunk`]: WaveFormatChunk
    sample_length: u32,
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct WaveDataChunk {
    data: Vec<u8>,
}

/// A chunk constaining a list of subchunks.
#[derive(PartialEq, Eq, Clone, Debug)]
struct ListChunk {
    /// A tag which helps identify what information the [`ListChunk`] holds, but this should not in
    /// any way change how the [`ListChunk`] is interpereted or parsed.
    ///
    /// [`ListChunk`]: ListChunk
    tag: [u8; 4],
    chunks: Vec<Chunk>,
}

/// A specific type of [`ListChunk`] which holds information about the file's contents.
///
/// [`ListChunk`]: ListChunk
#[derive(PartialEq, Eq, Clone, Debug)]
struct InfoListChunk(Vec<Chunk>);

impl ReadTag for InfoListChunk {
    fn album_title(&self) -> Option<String> {
        None
    }

    fn bpm(&self) -> Option<f64> {
        None
    }

    fn composer(&self) -> Option<String> {
        None
    }

    fn content_type(&self) -> Option<String> {
        None
    }

    fn copyright_message(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::Copyright(CopyrightChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn date(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::CreationDate(CreationDateChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn playlist_delay(&self) -> Option<String> {
        None
    }

    fn encoded_by(&self) -> Option<String> {
        todo!()
    }

    fn lyricist(&self) -> Option<String> {
        None
    }

    fn file_type(&self) -> Option<String> {
        None
    }

    fn time(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::DigitizationTime(DigitizationTimeChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn content_group_description(&self) -> Option<String> {
        None
    }

    fn title(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::Name(NameChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn subtitle(&self) -> Option<String> {
        None
    }

    fn initial_key(&self) -> Option<crate::audio_info::Key> {
        None
    }

    fn language(&self) -> Option<String> {
        None
    }

    fn length(&self) -> Option<String> {
        None
    }

    fn media_type(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::Medium(MediumChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn original_album(&self) -> Option<String> {
        None
    }

    fn original_filename(&self) -> Option<String> {
        None
    }

    fn original_artist(&self) -> Option<String> {
        None
    }

    fn original_release_year(&self) -> Option<u32> {
        None
    }

    fn file_owner(&self) -> Option<String> {
        None
    }

    fn lead_artist(&self) -> Option<String> {
        None
    }

    fn band(&self) -> Option<String> {
        None
    }

    fn conductor(&self) -> Option<String> {
        None
    }

    fn modified_by(&self) -> Option<String> {
        None
    }

    fn part_of_set(&self) -> Option<String> {
        let InfoListChunk(chunks) = self;
        chunks.iter().find_map(|chunk| match chunk {
            Chunk::TableOfContents(TableOfContentsChunk(text)) => Some(text.clone()),
            _ => None,
        })
    }

    fn publisher(&self) -> Option<String> {
        None
    }

    fn track_number(&self) -> Option<TrackNumber> {
        let InfoListChunk(chunks) = self;
        let text = chunks.iter().find_map(|chunk| match chunk {
            Chunk::TrackNumber(TrackNumberChunk(text)) => Some(text.clone()),
            _ => None,
        })?;

        Some(TrackNumber {
            track: text.parse().ok()?,
            of: None,
        })
    }

    fn recording_date(&self) -> Option<String> {
        None
    }

    fn internet_radio_station(&self) -> Option<String> {
        None
    }

    fn size(&self) -> Option<String> {
        None
    }

    fn isrc(&self) -> Option<String> {
        None
    }

    fn encoding_settings(&self) -> Option<String> {
        None
    }

    fn year(&self) -> Option<u32> {
        let InfoListChunk(chunks) = self;
        let text = chunks.iter().find_map(|chunk| match chunk {
            Chunk::DigitizationTime(DigitizationTimeChunk(text)) => Some(text.clone()),
            _ => None,
        })?;

        // `text` should be of format "Wed Jan 02 02:03:55 1990\n"
        //                      index: 0   4   8  11       20  24

        let year_text = &text[20..24];
        year_text.parse().ok()
    }
}

impl WriteTag for InfoListChunk {
    fn write_album_title(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_bpm(&mut self, _value: Option<f64>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_composer(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_content_type(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_copyright_message(
        &mut self,
        value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::Copyright(CopyrightChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::Copyright(CopyrightChunk(value)));
        }

        Ok(())
    }

    fn write_date(&mut self, value: Option<String>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::CreationDate(CreationDateChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::CreationDate(CreationDateChunk(value)));
        }

        Ok(())
    }

    fn write_playlist_delay(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_encoded_by(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_lyricist(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_file_type(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_time(&mut self, value: Option<String>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::DigitizationTime(DigitizationTimeChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::DigitizationTime(DigitizationTimeChunk(value)));
        }

        Ok(())
    }

    fn write_content_group_description(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_title(&mut self, value: Option<String>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::Name(NameChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::Name(NameChunk(value)));
        }

        Ok(())
    }

    fn write_subtitle(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_initial_key(
        &mut self,
        _value: Option<crate::audio_info::Key>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_language(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_length(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_media_type(&mut self, value: Option<String>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::Medium(MediumChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::Medium(MediumChunk(value)));
        }

        Ok(())
    }

    fn write_original_album(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_filename(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_artist(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_release_year(
        &mut self,
        _value: Option<u32>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_file_owner(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_lead_artist(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_band(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_conductor(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_modified_by(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_part_of_set(&mut self, value: Option<String>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::TableOfContents(TableOfContentsChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            *text = value;
        } else {
            chunks.push(Chunk::TableOfContents(TableOfContentsChunk(value)));
        }

        Ok(())
    }

    fn write_publisher(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_track_number(
        &mut self,
        value: Option<TrackNumber>,
    ) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v,
        };

        let track_number = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::TrackNumber(TrackNumberChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = track_number {
            *text = value.track.to_string();
        } else {
            chunks.push(Chunk::TrackNumber(TrackNumberChunk(
                value.track.to_string(),
            )));
        }

        Ok(())
    }

    fn write_recording_date(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_internet_radio_station(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_size(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_isrc(&mut self, _value: Option<String>) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_encoding_settings(
        &mut self,
        _value: Option<String>,
    ) -> result::Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_year(&mut self, value: Option<u32>) -> result::Result<(), WriteTagError> {
        let InfoListChunk(chunks) = self;

        let value = match value {
            None => {
                let maybe_pos = chunks.iter().position(|chunk| match chunk {
                    Chunk::Copyright(_) => true,
                    _ => false,
                });

                if let Some(pos) = maybe_pos {
                    chunks.remove(pos);
                }

                return Ok(());
            }

            Some(v) => v.to_string(),
        };

        let text_res = chunks.iter_mut().find_map(|chunk| match chunk {
            Chunk::TableOfContents(TableOfContentsChunk(text)) => Some(text),
            _ => None,
        });

        if let Some(text) = text_res {
            text.truncate(20);
            text.push_str(&value);
        } else {
            let mut text = "Mon Jan 01 12:00:00 ".to_string();
            text.push_str(&value);
            chunks.push(Chunk::TableOfContents(TableOfContentsChunk(text)));
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct ArchivalLocationChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct ArtistChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct CommissionedChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct CommentsChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct CopyrightChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct CreationDateChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct CroppedChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct DimensionsChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct DotsPerInchChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct EngineerChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct GenreChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct KeywordsChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct LightnessSettingsChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct MediumChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct NameChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct PaletteSettingsChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct ProductChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct DescriptionChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct SoftwareChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct SharpnessChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct SourceChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct SourceFormChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct TechnicianChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct SmpteTimeCodeChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct DigitizationTimeChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct TrackNumberChunk(String);

#[derive(PartialEq, Eq, Clone, Debug)]
struct TableOfContentsChunk(String);

/// An [`Iterator`] over WAVE RIFF [`Chunk`]s.
///
/// [`Iterator`]: Iterator
/// [`Chunk`]: Chunk
struct ChunksIter<'b> {
    /// Remaining bytes from last read.
    remaining_bytes: &'b [u8],
}

impl<'b> Iterator for ChunksIter<'b> {
    type Item = Chunk;

    fn next(&mut self) -> Option<Self::Item> {
        let (typeless, remaining_bytes) = TypelessChunk::from_bytes(self.remaining_bytes)?;
        self.remaining_bytes = remaining_bytes;
        typeless.typed()
    }
}

/// The format of a wave file, plus the extension that would typically follow the `'fmt '` chunk.
/// Since it minimizes the number of invalid configurations, the extension and format tag are just
/// encoded together here, which also makes the extension size field redundant.
#[derive(PartialEq, Eq, Clone, Debug)]
enum WaveFormat {
    // From Antex Electronics:
    // Microsoft's 1994 document states that G.721 and G.723's header formats are "essentially the
    // same".
    AntexElectronicsADPCMG723 {
        aux_block_size: u16,
    },
    AntexElectronicsADPCME,
    AntexElectronicsADPCMG721 {
        aux_block_size: u16,
    },

    // From Audio Processing Technology:
    AudioProcessingTechnology,

    // From Audiofile:
    AudioFileAF36,
    AudioFileAF10,

    // From Controls Resouces Limited:
    ControlsResourcesVQLPC {
        compression_type: u16,
    },
    ControlsResourcesCR10,

    // From Creative Labs:
    CreativeLabsADPCM {
        revision: u16,
    },
    CreativeLabsFastSpeech8 {
        revision: u16,
    },
    CreativeLabsFastSpeech10 {
        revision: u16,
    },

    // From Dolby:
    DolbyAC2 {
        /// Indicates the number of aux bits in a block:
        /// - 0 -> 0 bits
        /// - 1 -> 8 bits
        /// - 2 -> 16 bits
        /// - 3 -> 32 bits
        aux_bits_code: u16,
    },

    // From DSP Group:
    DspGroupTrueSpeech {
        revision: u16,
        samples_per_block: u32,
        // There are supposedly more field, which are proprietary, that would equate to a total
        // format extension size of 32 bytes.
        proprietary_fields: [u8; 32 - 2 - 4],
    },

    // From DSP Solutions:
    DspSolutionsDigiSTD,
    DspSolutionsDigiFIX,
    DspSolutionsDigiREAL,
    DspSolutionsDigiADPCM,

    // From Echo Speech:
    EchoSpeechSC1,

    // From Fujitsu:
    FujitsuSND,

    // From IBM:
    IbmCVSD,

    // From Ing C. Olivetti & C., S.p.A.:
    OlivettiGSM,
    OlivettiADPCM,
    OlivettiCELP,
    OlivettiSBC,
    OlivettiOPR,

    // From Intel:
    // Intel's two formats are appearantly identical in all but name, and Microsoft's RIFF and
    // Waveform documentation provides the same constant value for both their tags. Given that we
    // have no way to distinguish which one was the file creator's intention, nor a reason to care,
    // I just list the one here.
    IntelDviADPCM {
        samples_per_block: u16,
    },

    // From Microsoft:
    // I genuinely can't tell is GSM610 is supposed to have another `WORD` field for samples per
    // block or not, Microsoft's docs show a field in the struct typedef, but not in the table, and
    // the format extension size (`sbSize`) is non-constant here so... ¯\_(ツ)_/¯
    //
    // MPEG has a bunch of stuff going on, I'm hoping I can defer to the MP3 code I've already
    // written cause that shit was annoying.
    Unknown,
    MicrosoftPCM,
    MicrosoftADPCM {
        samples_per_block: u16,
        // Noted here since its part of the struct in memory, but removed since it would be encoded
        // in the `Vec` for `coefficient_sets`.
        // num_coefficient_sets: u16,
        coefficient_sets: Vec<AdpcmCoefficientSet>,
    },
    MicrosoftALaw,
    MicrosoftMuLaw,
    MicrosoftGSM610,
    MicrosoftMPEG,

    // From Natural MicroSystems:
    NaturalMicroSystemsVbxADPCM,

    // From OKI:
    OkiADPCM {
        pole: u16,
    },

    // From Sierra Semiconductor Corp:
    SierraADPCM {
        revision: u16,
    },

    // From Speech Compression:
    SpeechCompressionSONARC {
        compression_type: u16,
    },

    // From Videologic:
    VideoLogicMediaSpaceADPCM {
        revision: u16,
    },

    // From Yamaha:
    YamahaADPCM,
}

impl WaveFormat {
    /// Read the extension associated with the given [`WaveFormatTag`] and return it. If the given
    /// tag does not have a format extension, the returned associated enum variant will simply have
    /// no fields.
    ///
    /// [`WaveFormatTag`]: WaveFormatTag
    fn read_format_extension(
        format_tag: WaveFormatTag,
        extension_size: usize,
        bytes: &[u8],
    ) -> Option<WaveFormat> {
        match format_tag {
            WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG723 => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let aux_block_size = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::AntexElectronicsADPCMG723 { aux_block_size })
            }

            WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCME => Some(WaveFormat::AntexElectronicsADPCME),

            WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG721 => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let aux_block_size = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::AntexElectronicsADPCMG721 { aux_block_size })
            }

            WAVE_FORMAT_AUDIO_PROCESSING_TECHNOLOGY => Some(WaveFormat::AudioProcessingTechnology),
            WAVE_FORMAT_AUDIO_FILE_AF36 => Some(WaveFormat::AudioFileAF36),
            WAVE_FORMAT_AUDIO_FILE_AF10 => Some(WaveFormat::AudioFileAF10),

            WAVE_FORMAT_CONTROLS_RESOURCES_VQLPC => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let compression_type = u16::from_le_bytes(*bytes[0..2].as_array()?);

                // "This value is reserved and should be set to one"
                if compression_type != 1 {
                    return None;
                }

                Some(WaveFormat::ControlsResourcesVQLPC { compression_type })
            }

            WAVE_FORMAT_CONTROLS_RESOURCES_CR10 => Some(WaveFormat::ControlsResourcesCR10),

            WAVE_FORMAT_CREATIVE_LABS_ADPCM => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::CreativeLabsADPCM { revision })
            }

            WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_8 => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::CreativeLabsFastSpeech8 { revision })
            }

            WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_10 => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::CreativeLabsFastSpeech10 { revision })
            }

            WAVE_FORMAT_DOLBY_AC2 => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let aux_bits_code = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::DolbyAC2 { aux_bits_code })
            }

            WAVE_FORMAT_DSP_GROUP_TRUE_SPEECH => {
                if extension_size != 32 {
                    return None;
                }

                if bytes.len() < 32 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                let samples_per_block = u32::from_le_bytes(*bytes[2..6].as_array()?);
                let proprietary_fields = *bytes[6..32].as_array()?;

                Some(WaveFormat::DspGroupTrueSpeech {
                    revision,
                    samples_per_block,
                    proprietary_fields,
                })
            }

            WAVE_FORMAT_DSP_SOLUTIONS_DIGI_STD => Some(WaveFormat::DspSolutionsDigiSTD),
            WAVE_FORMAT_DSP_SOLUTIONS_DIGI_FIX => Some(WaveFormat::DspSolutionsDigiFIX),
            WAVE_FORMAT_DSP_SOLUTIONS_DIGI_REAL => Some(WaveFormat::DspSolutionsDigiREAL),
            WAVE_FORMAT_DSP_SOLUTIONS_DIGI_ADPCM => Some(WaveFormat::DspSolutionsDigiADPCM),
            WAVE_FORMAT_ECHO_SPEECH_SC1 => Some(WaveFormat::EchoSpeechSC1),
            WAVE_FORMAT_FUJITSU_SND => Some(WaveFormat::FujitsuSND),
            WAVE_FORMAT_IBM_CVSD => Some(WaveFormat::IbmCVSD),
            WAVE_FORMAT_OLIVETTI_GSM => Some(WaveFormat::OlivettiGSM),
            WAVE_FORMAT_OLIVETTI_ADPCM => Some(WaveFormat::OlivettiADPCM),
            WAVE_FORMAT_OLIVETTI_CELP => Some(WaveFormat::OlivettiCELP),
            WAVE_FORMAT_OLIVETTI_SBC => Some(WaveFormat::OlivettiSBC),
            WAVE_FORMAT_OLIVETTI_OPR => Some(WaveFormat::OlivettiOPR),

            WAVE_FORMAT_UNKNOWN => Some(WaveFormat::Unknown),

            WAVE_FORMAT_MICROSOFT_PCM => Some(WaveFormat::MicrosoftPCM),

            WAVE_FORMAT_MICROSOFT_ADPCM => {
                if bytes.len() < 4 {
                    return None;
                }

                let samples_per_block = u16::from_le_bytes(*bytes[0..2].as_array()?);
                let num_coefficient_sets = u16::from_le_bytes(*bytes[2..4].as_array()?);

                if bytes.len() < 4 + num_coefficient_sets as usize {
                    return None;
                }

                let mut remaining = &bytes[4..];
                let mut coefficient_sets: Vec<AdpcmCoefficientSet> = Vec::new();

                while coefficient_sets.len() < num_coefficient_sets as _ {
                    let set = AdpcmCoefficientSet(
                        i32::from_le_bytes(*remaining[0..4].as_array()?),
                        i32::from_le_bytes(*remaining[4..8].as_array()?),
                    );

                    coefficient_sets.push(set);
                    remaining = &remaining[8..];
                }

                Some(WaveFormat::MicrosoftADPCM {
                    samples_per_block,
                    coefficient_sets,
                })
            }

            WAVE_FORMAT_MICROSOFT_ALAW => Some(WaveFormat::MicrosoftALaw),
            WAVE_FORMAT_MICROSOFT_MULAW => Some(WaveFormat::MicrosoftMuLaw),
            WAVE_FORMAT_MICROSOFT_GSM610 => Some(WaveFormat::MicrosoftGSM610),
            WAVE_FORMAT_MICROSOFT_MPEG => Some(WaveFormat::MicrosoftMPEG),
            WAVE_FORMAT_NATURAL_MICRO_SYSTEMS_VBX_ADPCM => {
                Some(WaveFormat::NaturalMicroSystemsVbxADPCM)
            }

            // Both intel formats are the same.
            WAVE_FORMAT_INTEL_DVI_ADPCM => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let samples_per_block = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::IntelDviADPCM { samples_per_block })
            }

            WAVE_FORMAT_OKI_ADPCM => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let pole = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::OkiADPCM { pole })
            }

            WAVE_FORMAT_SIERRA_ADPCM => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::SierraADPCM { revision })
            }

            WAVE_FORMAT_SPEECH_COMPRESSION_SONARC => {
                if extension_size != 2 {
                    return None;
                }

                if bytes.len() < 2 {
                    return None;
                }

                let compression_type = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::SpeechCompressionSONARC { compression_type })
            }

            WAVE_FORMAT_VIDEO_LOGIC_MEDIA_SPACE_ADPCM => {
                if bytes.len() < 2 {
                    return None;
                }

                let revision = u16::from_le_bytes(*bytes[0..2].as_array()?);
                Some(WaveFormat::VideoLogicMediaSpaceADPCM { revision })
            }

            WAVE_FORMAT_YAMAHA_ADPCM => Some(WaveFormat::YamahaADPCM),

            _ => None,
        }
    }

    /// Get the constant value tag used to indicate the given [`WaveFormat`].
    ///
    /// [`WaveFormat`]: WaveFormat
    fn format_tag(&self) -> WaveFormatTag {
        match self {
            WaveFormat::AntexElectronicsADPCMG723 { .. } => WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG723,
            WaveFormat::AntexElectronicsADPCME => WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCME,
            WaveFormat::AntexElectronicsADPCMG721 { .. } => WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG721,
            WaveFormat::AudioProcessingTechnology => WAVE_FORMAT_AUDIO_PROCESSING_TECHNOLOGY,
            WaveFormat::AudioFileAF36 => WAVE_FORMAT_AUDIO_FILE_AF36,
            WaveFormat::AudioFileAF10 => WAVE_FORMAT_AUDIO_FILE_AF10,
            WaveFormat::ControlsResourcesVQLPC { .. } => WAVE_FORMAT_CONTROLS_RESOURCES_VQLPC,
            WaveFormat::ControlsResourcesCR10 => WAVE_FORMAT_CONTROLS_RESOURCES_CR10,
            WaveFormat::CreativeLabsADPCM { .. } => WAVE_FORMAT_CREATIVE_LABS_ADPCM,
            WaveFormat::CreativeLabsFastSpeech8 { .. } => WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_8,
            WaveFormat::CreativeLabsFastSpeech10 { .. } => WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_10,
            WaveFormat::DolbyAC2 { .. } => WAVE_FORMAT_DOLBY_AC2,
            WaveFormat::DspGroupTrueSpeech { .. } => WAVE_FORMAT_DSP_GROUP_TRUE_SPEECH,
            WaveFormat::DspSolutionsDigiSTD => WAVE_FORMAT_DSP_SOLUTIONS_DIGI_STD,
            WaveFormat::DspSolutionsDigiFIX => WAVE_FORMAT_DSP_SOLUTIONS_DIGI_FIX,
            WaveFormat::DspSolutionsDigiREAL => WAVE_FORMAT_DSP_SOLUTIONS_DIGI_REAL,
            WaveFormat::DspSolutionsDigiADPCM => WAVE_FORMAT_DSP_SOLUTIONS_DIGI_ADPCM,
            WaveFormat::EchoSpeechSC1 => WAVE_FORMAT_ECHO_SPEECH_SC1,
            WaveFormat::FujitsuSND => WAVE_FORMAT_FUJITSU_SND,
            WaveFormat::IbmCVSD => WAVE_FORMAT_IBM_CVSD,
            WaveFormat::OlivettiGSM => WAVE_FORMAT_OLIVETTI_GSM,
            WaveFormat::OlivettiADPCM => WAVE_FORMAT_OLIVETTI_ADPCM,
            WaveFormat::OlivettiCELP => WAVE_FORMAT_OLIVETTI_CELP,
            WaveFormat::OlivettiSBC => WAVE_FORMAT_OLIVETTI_SBC,
            WaveFormat::OlivettiOPR => WAVE_FORMAT_OLIVETTI_OPR,
            WaveFormat::IntelDviADPCM { .. } => WAVE_FORMAT_INTEL_DVI_ADPCM,
            WaveFormat::Unknown => WAVE_FORMAT_UNKNOWN,
            WaveFormat::MicrosoftPCM => WAVE_FORMAT_MICROSOFT_PCM,
            WaveFormat::MicrosoftADPCM { .. } => WAVE_FORMAT_MICROSOFT_ADPCM,
            WaveFormat::MicrosoftALaw => WAVE_FORMAT_MICROSOFT_ALAW,
            WaveFormat::MicrosoftMuLaw => WAVE_FORMAT_MICROSOFT_MULAW,
            WaveFormat::MicrosoftGSM610 => WAVE_FORMAT_MICROSOFT_GSM610,
            WaveFormat::MicrosoftMPEG => WAVE_FORMAT_MICROSOFT_MPEG,
            WaveFormat::NaturalMicroSystemsVbxADPCM => WAVE_FORMAT_NATURAL_MICRO_SYSTEMS_VBX_ADPCM,
            WaveFormat::OkiADPCM { .. } => WAVE_FORMAT_OKI_ADPCM,
            WaveFormat::SierraADPCM { .. } => WAVE_FORMAT_SIERRA_ADPCM,
            WaveFormat::SpeechCompressionSONARC { .. } => WAVE_FORMAT_SPEECH_COMPRESSION_SONARC,
            WaveFormat::VideoLogicMediaSpaceADPCM { .. } => {
                WAVE_FORMAT_VIDEO_LOGIC_MEDIA_SPACE_ADPCM
            }
            WaveFormat::YamahaADPCM => WAVE_FORMAT_YAMAHA_ADPCM,
        }
    }

    fn to_bytes(self) -> (WaveFormatTag, Vec<u8>) {
        let mut bytes = Vec::new();
        let tag = self.format_tag();

        match self {
            WaveFormat::AntexElectronicsADPCMG723 { aux_block_size } => {
                bytes.append(&mut aux_block_size.to_le_bytes().to_vec())
            }
            WaveFormat::AntexElectronicsADPCMG721 { aux_block_size } => {
                bytes.append(&mut aux_block_size.to_le_bytes().to_vec())
            }
            WaveFormat::ControlsResourcesVQLPC { compression_type } => {
                bytes.append(&mut compression_type.to_le_bytes().to_vec())
            }
            WaveFormat::CreativeLabsADPCM { revision } => {
                bytes.append(&mut revision.to_le_bytes().to_vec())
            }
            WaveFormat::CreativeLabsFastSpeech8 { revision } => {
                bytes.append(&mut revision.to_le_bytes().to_vec())
            }
            WaveFormat::CreativeLabsFastSpeech10 { revision } => {
                bytes.append(&mut revision.to_le_bytes().to_vec())
            }
            WaveFormat::DolbyAC2 { aux_bits_code } => {
                bytes.append(&mut aux_bits_code.to_le_bytes().to_vec())
            }
            WaveFormat::DspGroupTrueSpeech {
                revision,
                samples_per_block,
                proprietary_fields,
            } => {
                bytes.append(&mut revision.to_le_bytes().to_vec());
                bytes.append(&mut samples_per_block.to_le_bytes().to_vec());
                bytes.append(&mut proprietary_fields.to_vec());
            }
            WaveFormat::IntelDviADPCM { samples_per_block } => {
                bytes.append(&mut samples_per_block.to_le_bytes().to_vec())
            }
            WaveFormat::MicrosoftADPCM {
                samples_per_block,
                coefficient_sets,
            } => {
                bytes.append(&mut samples_per_block.to_le_bytes().to_vec());
                bytes.append(
                    &mut coefficient_sets
                        .into_iter()
                        .flat_map(|AdpcmCoefficientSet(a, b)| {
                            let a_bytes = a.to_le_bytes();
                            let b_bytes = b.to_le_bytes();

                            [
                                a_bytes[0], a_bytes[1], a_bytes[2], a_bytes[3], b_bytes[0],
                                b_bytes[1], b_bytes[2], b_bytes[3],
                            ]
                        })
                        .collect(),
                );
            }
            WaveFormat::OkiADPCM { pole } => bytes.append(&mut pole.to_le_bytes().to_vec()),
            WaveFormat::SierraADPCM { revision } => {
                bytes.append(&mut revision.to_le_bytes().to_vec())
            }
            WaveFormat::SpeechCompressionSONARC { compression_type } => {
                bytes.append(&mut compression_type.to_le_bytes().to_vec())
            }
            WaveFormat::VideoLogicMediaSpaceADPCM { revision } => {
                bytes.append(&mut revision.to_le_bytes().to_vec())
            }
            _ => (),
        }

        (tag, bytes)
    }
}

type WaveFormatTag = u16;

// From Antex Electronics:
const WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG723: WaveFormatTag = 0x0014;
const WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCME: WaveFormatTag = 0x0033;
const WAVE_FORMAT_ANTEX_ELECTRONICS_ADPCMG721: WaveFormatTag = 0x0040;

// From Audio Processing Technology:
const WAVE_FORMAT_AUDIO_PROCESSING_TECHNOLOGY: WaveFormatTag = 0x0025;

// From Audiofile:
const WAVE_FORMAT_AUDIO_FILE_AF36: WaveFormatTag = 0x0024;
const WAVE_FORMAT_AUDIO_FILE_AF10: WaveFormatTag = 0x0026;

// From Controls Resouces Limited:
const WAVE_FORMAT_CONTROLS_RESOURCES_VQLPC: WaveFormatTag = 0x0034;
const WAVE_FORMAT_CONTROLS_RESOURCES_CR10: WaveFormatTag = 0x0037;

// From Creative Labs:
const WAVE_FORMAT_CREATIVE_LABS_ADPCM: WaveFormatTag = 0x0200;
const WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_8: WaveFormatTag = 0x0202;
const WAVE_FORMAT_CREATIVE_LABS_FASTSPEECH_10: WaveFormatTag = 0x0203;

// From Dolby:
const WAVE_FORMAT_DOLBY_AC2: WaveFormatTag = 0x0030;

// From DSP Group:
const WAVE_FORMAT_DSP_GROUP_TRUE_SPEECH: WaveFormatTag = 0x0022;

// From DSP Solutions:
const WAVE_FORMAT_DSP_SOLUTIONS_DIGI_STD: WaveFormatTag = 0x0015;
const WAVE_FORMAT_DSP_SOLUTIONS_DIGI_FIX: WaveFormatTag = 0x0016;
const WAVE_FORMAT_DSP_SOLUTIONS_DIGI_REAL: WaveFormatTag = 0x0035;
const WAVE_FORMAT_DSP_SOLUTIONS_DIGI_ADPCM: WaveFormatTag = 0x0036;

// From Echo Speech:
const WAVE_FORMAT_ECHO_SPEECH_SC1: WaveFormatTag = 0x0023;

// From Fujitsu:
const WAVE_FORMAT_FUJITSU_SND: WaveFormatTag = 0x0300;

// From IBM:
const WAVE_FORMAT_IBM_CVSD: WaveFormatTag = 0x0005;

// From Ing C. Olivetti & C., S.p.A.:
const WAVE_FORMAT_OLIVETTI_GSM: WaveFormatTag = 0x1000;
const WAVE_FORMAT_OLIVETTI_ADPCM: WaveFormatTag = 0x1001;
const WAVE_FORMAT_OLIVETTI_CELP: WaveFormatTag = 0x1002;
const WAVE_FORMAT_OLIVETTI_SBC: WaveFormatTag = 0x1003;
const WAVE_FORMAT_OLIVETTI_OPR: WaveFormatTag = 0x1004;

// From Intel:
// Intel's two formats are appearantly identical in all but name, and Microsoft's RIFF and Waveform
// documentation provides the same constant value for both their tags.
// const WAVE_FORMAT_INTEL_IMA_ADPCM: WaveFormatTag = 0x0011;
const WAVE_FORMAT_INTEL_DVI_ADPCM: WaveFormatTag = 0x0011;

// From Microsoft:
const WAVE_FORMAT_UNKNOWN: WaveFormatTag = 0x0000;
const WAVE_FORMAT_MICROSOFT_PCM: WaveFormatTag = 0x0001;
const WAVE_FORMAT_MICROSOFT_ADPCM: WaveFormatTag = 0x0002;
const WAVE_FORMAT_MICROSOFT_ALAW: WaveFormatTag = 0x0006;
const WAVE_FORMAT_MICROSOFT_MULAW: WaveFormatTag = 0x0007;
const WAVE_FORMAT_MICROSOFT_GSM610: WaveFormatTag = 0x0031;
const WAVE_FORMAT_MICROSOFT_MPEG: WaveFormatTag = 0x0050;

// From Natural MicroSystems:
const WAVE_FORMAT_NATURAL_MICRO_SYSTEMS_VBX_ADPCM: WaveFormatTag = 0x0038;

// From OKI:
const WAVE_FORMAT_OKI_ADPCM: WaveFormatTag = 0x0010;

// From Sierra Semiconductor Corp:
const WAVE_FORMAT_SIERRA_ADPCM: WaveFormatTag = 0x0013;

// From Speech Compression:
const WAVE_FORMAT_SPEECH_COMPRESSION_SONARC: WaveFormatTag = 0x0021;

// From Videologic:
const WAVE_FORMAT_VIDEO_LOGIC_MEDIA_SPACE_ADPCM: WaveFormatTag = 0x0012;

// From Yamaha:
const WAVE_FORMAT_YAMAHA_ADPCM: WaveFormatTag = 0x0020;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
struct AdpcmCoefficientSet(i32, i32);

/// A chunk containing text. This isn't really a typed chunk yet, just a common data format between
/// many chunks.
#[derive(Debug, Clone, Eq, PartialEq)]
struct TextChunk {
    id: ChunkId,
    text: String,
}

impl TextChunk {
    /// Take the given [`TypelessChunk`] to be a ZSTR chunk containing a null-terminated string.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn from_zstr_chunk(mut chunk: TypelessChunk) -> Option<TextChunk> {
        if *chunk.data.last()? != b'\0' {
            return None;
        }

        // remove the null character
        chunk.data.pop();

        Some(TextChunk {
            id: chunk.id,
            text: String::from_utf8(chunk.data).ok()?,
        })
    }

    /// Turn the [`TextChunk`] into a [`TypelessChunk`]. This conversion requires that the
    /// [`TextChunk`]'s `text` field contains valid ASCII.
    ///
    /// [`TextChunk`]: TextChunk
    /// [`TypelessChunk`]: TypelessChunk
    fn detype(self) -> Result<TypelessChunk> {
        let ascii = self.text.as_ascii().ok_or(ParseError::NonAsciiString)?;

        Ok(TypelessChunk {
            id: self.id,
            data: ascii
                .into_iter()
                .map(|ascii_char| ascii_char.to_u8())
                .collect(),
        })
    }
}

/// A chunk without a yet interpereted type or data format.
#[derive(Debug, Clone, Eq, PartialEq)]
struct TypelessChunk {
    /// The chunk's ID, which should be used to determing how to interperet the contents of the
    /// `data` field.
    id: ChunkId,
    data: Vec<u8>,
}

impl TypelessChunk {
    /// Reads a single [`TypelessChunk`] and returns it, along with the remaining bytes of the ones
    /// given. Gives [`None`] if there are not enough bytes.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`None`]: Option::None
    fn from_bytes(bytes: &[u8]) -> Option<(TypelessChunk, &[u8])> {
        let id = *bytes[0..4].as_array()?;
        let size = u32::from_le_bytes(*bytes[4..8].as_array()?) as usize;
        let data = bytes[8..8 + size].to_vec();

        Some((TypelessChunk { id, data }, &bytes[8 + size..]))
    }

    /// Turn this [`TypelessChunk`] into bytes which can be parsed back into a [`TypelessChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn to_bytes(mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.id.to_vec());
        bytes.append(&mut (self.data.len() as u32).to_le_bytes().to_vec());
        bytes.append(&mut self.data);
        bytes
    }

    /// Read the given [`TypelessChunk`]'s `id` field and attempt to interperet it as the associated
    /// chunk type.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn typed(self) -> Option<Chunk> {
        match self.id {
            ID_RIFF => Some(Chunk::Riff(self.riff_chunk()?)),
            ID_WAVE_FORMAT => Some(Chunk::WaveFormat(self.wave_format_chunk()?)),
            ID_JUNK => Some(Chunk::Junk(self.junk_chunk())),
            ID_PAD => Some(Chunk::Pad(self.pad_chunk())),
            ID_FACT => Some(Chunk::Fact(self.fact_chunk()?)),
            ID_WAVE_DATA => Some(Chunk::WaveData(WaveDataChunk { data: self.data })),
            ID_LIST => {
                let list = self.list_chunk()?;

                match list.tag {
                    [b'I', b'N', b'F', b'O'] => Some(Chunk::InfoList(InfoListChunk(list.chunks))),
                    _ => Some(Chunk::List(list)),
                }
            }

            ID_ARCHIVAL_LOCATION => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::ArchivalLocation(ArchivalLocationChunk(text)))
            }
            ID_ARTIST => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Artist(ArtistChunk(text)))
            }
            ID_COMMISSIONED => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Commissioned(CommissionedChunk(text)))
            }
            ID_COMMENTS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Comments(CommentsChunk(text)))
            }
            ID_COPYRIGHT => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Copyright(CopyrightChunk(text)))
            }
            ID_CREATION_DATE => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::CreationDate(CreationDateChunk(text)))
            }
            ID_CROPPED => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Cropped(CroppedChunk(text)))
            }
            ID_DOTS_PER_INCH => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::DotsPerInch(DotsPerInchChunk(text)))
            }
            ID_DIMENSIONS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Dimensions(DimensionsChunk(text)))
            }
            ID_ENGINEER => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Engineer(EngineerChunk(text)))
            }
            ID_GENRE => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Genre(GenreChunk(text)))
            }
            ID_KEYWORDS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Keywords(KeywordsChunk(text)))
            }
            ID_LIGHTNESS_SETTINGS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::LightnessSettings(LightnessSettingsChunk(text)))
            }
            ID_MEDIUM => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Medium(MediumChunk(text)))
            }
            ID_NAME => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Name(NameChunk(text)))
            }
            ID_PALETTE_SETTINGS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::PaletteSettings(PaletteSettingsChunk(text)))
            }
            ID_PRODUCT => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Product(ProductChunk(text)))
            }
            ID_DESCRIPTION => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Description(DescriptionChunk(text)))
            }
            ID_SOFTWARE => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Software(SoftwareChunk(text)))
            }
            ID_SHARPNESS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Sharpness(SharpnessChunk(text)))
            }
            ID_SOURCE => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Source(SourceChunk(text)))
            }
            ID_SOURCE_FORM => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::SourceForm(SourceFormChunk(text)))
            }
            ID_TECHNICIAN => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::Technician(TechnicianChunk(text)))
            }
            ID_SMPTE_TIME_CODE => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::SmpteTimeCode(SmpteTimeCodeChunk(text)))
            }
            ID_DIGITIZATION_TIME => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::DigitizationTime(DigitizationTimeChunk(text)))
            }
            ID_TRACK_NUMBER => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::TrackNumber(TrackNumberChunk(text)))
            }
            ID_TABLE_OF_CONTENTS => {
                let TextChunk { text, .. } = TextChunk::from_zstr_chunk(self)?;
                Some(Chunk::TableOfContents(TableOfContentsChunk(text)))
            }

            _ => Some(Chunk::Typeless(self)),
        }
    }

    /// Attempt to interperet this [`TypelessChunk`] as a [`RiffChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`RiffChunk`]: RiffChunk
    fn riff_chunk(self) -> Option<RiffChunk> {
        if self.data.len() < 4 {
            return None;
        }

        let tag: [u8; 4] = *self.data[0..4].as_array()?;
        let chunks: Vec<Chunk> = ChunksIter {
            remaining_bytes: &self.data[4..],
        }
        .collect();

        Some(RiffChunk { tag, chunks })
    }

    /// Attempt to interperet this [`TypelessChunk`] as a [`WaveFormatChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`WaveFormatChunk`]: WaveFormatChunk
    fn wave_format_chunk(self) -> Option<WaveFormatChunk> {
        let bytes = &self.data;

        if bytes.len() < 18 {
            return None;
        }

        // Skipping the format tag for now
        let channels = u16::from_le_bytes(*bytes[2..4].as_array()?);
        let samples_per_second = u32::from_le_bytes(*bytes[4..8].as_array()?);
        let average_bytes_per_second = u32::from_le_bytes(*bytes[8..12].as_array()?);
        let block_align = u16::from_le_bytes(*bytes[12..14].as_array()?);
        let bits_per_sample = u16::from_le_bytes(*bytes[14..16].as_array()?);

        let format_tag = u16::from_le_bytes(*bytes[0..2].as_array()?);
        let extension_size = u16::from_le_bytes(*bytes[16..18].as_array()?) as usize;
        let format = WaveFormat::read_format_extension(format_tag, extension_size, &bytes[18..])?;

        Some(WaveFormatChunk {
            format,
            channels,
            samples_per_second,
            average_bytes_per_second,
            block_align,
            bits_per_sample,
        })
    }

    /// Attempt to interperet this [`TypelessChunk`] as a [`FactChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`FactChunk`]: FactChunk
    fn fact_chunk(self) -> Option<FactChunk> {
        if self.data.len() < 4 {
            return None;
        }

        Some(FactChunk {
            sample_length: u32::from_le_bytes(*self.data[0..4].as_array()?),
        })
    }

    /// Interperet this [`TypelessChunk`] as a [`JunkChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`JunkChunk`]: JunkChunk
    fn junk_chunk(self) -> JunkChunk {
        JunkChunk(self.data)
    }

    /// Interperet this [`TypelessChunk`] as a [`PadChunk`].
    ///
    /// [`TypelessChunk`]: TypelessChunk
    /// [`PadChunk`]: PadChunk
    fn pad_chunk(self) -> PadChunk {
        PadChunk(self.data)
    }

    fn list_chunk(self) -> Option<ListChunk> {
        let tag = *self.data[0..4].as_array()?;
        let chunks: Vec<Chunk> = ChunksIter {
            remaining_bytes: &self.data[4..],
        }
        .collect();

        Some(ListChunk { tag, chunks })
    }
}
