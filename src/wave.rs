// NOTE: Waveform files are appearantly little endian :(

use std::{error::Error, fmt::Display, fs, io, path::Path, result};

type ChunkId = [u8; 4];

const ID_RIFF: ChunkId = *b"RIFF";
const ID_PAD: ChunkId = *b"PAD ";
const ID_JUNK: ChunkId = *b"JUNK";
const ID_FACT: ChunkId = *b"fact";
const ID_FORMAT: ChunkId = *b"fmt ";
const ID_WAVE_DATA: ChunkId = *b"data";

/// A RIFF file, which here we assume is a WAVE file, but it technically doesn't have to be. In the
/// case that a non-WAVE RIFF file is parsed, its audio-related functions will fail as they would if
/// the file is malformed.
pub struct File {
    /// The top level [`RiffChunk`] containing all this RIFF's other [`Chunk`]s.
    ///
    /// [`RiffChunk`]: RiffChunk
    /// [`Chunk`]: Chunk
    riff: RiffChunk,
}

impl File {
    /// Read a new [`wave::File`] from a file at the given [`AsRef<Path>`].
    ///
    /// [`wave::File`]: File
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn read_from(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|io_err| ParseError::Io(io_err))?;
        File::from_bytes(&bytes)
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
    Io(io::Error),
    ExcessTopLevelChunks,
    ExpectedRiffChunk,
    ExpectedFormatChunk,
    ExpectedWaveDataChunk,
    FormatNotBeforeData,
    ExpectedFactChunk,
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

#[derive(Debug, Clone, Eq, PartialEq)]
struct TypelessChunk {
    id: [u8; 4],
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

    /// Read the given [`TypelessChunk`]'s `id` field and attempt to interperet it as the associated
    /// chunk type.
    ///
    /// [`TypelessChunk`]: TypelessChunk
    fn typed(self) -> Option<Chunk> {
        match self.id {
            ID_RIFF => Some(Chunk::Riff(self.riff_chunk()?)),
            ID_FORMAT => Some(Chunk::WaveFormat(self.wave_format_chunk()?)),
            ID_JUNK => Some(Chunk::Junk(self.junk_chunk())),
            ID_PAD => Some(Chunk::Pad(self.pad_chunk())),
            ID_FACT => Some(Chunk::Fact(self.fact_chunk()?)),
            ID_WAVE_DATA => Some(Chunk::WaveData(WaveDataChunk { data: self.data })),
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
}
