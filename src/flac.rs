use crate::audio_info::{self, Audio, ReadTag};
use std::{collections::HashMap, error::Error, fmt::Display, fs, io, path::Path, result};

/// A FLAC file.
pub struct File {
    metadata_blocks: Vec<MetadataBlock>,
    frame_data: Vec<u8>,
}

impl File {
    /// Read a FLAC file from the given [`AsRef<Path>`].
    ///
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn read_from(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|io_err| ParseError::Io(io_err))?;
        File::from_bytes(&bytes)
    }

    /// Parse a FLAC file from the given bytes.
    fn from_bytes(bytes: &[u8]) -> Result<File> {
        if bytes.len() < 4 {
            return Err(ParseError::NotEnoughBytes);
        }

        let marker = *bytes[0..4].as_array().ok_or(ParseError::NotEnoughBytes)?;
        let bytes = &bytes[4..];

        if marker != [b'f', b'L', b'a', b'C'] {
            return Err(ParseError::MissingMarker);
        }

        let mut metadata_blocks: Vec<MetadataBlock> = Vec::new();
        let mut blocks_iter = BlocksIter {
            bytes,
            finished: false,
        };

        while let Some(block) = blocks_iter.next() {
            metadata_blocks.push(block);
        }

        let frame_data = blocks_iter.bytes.to_vec();

        Ok(File {
            metadata_blocks,
            frame_data,
        })
    }

    fn stream_info_block(&self) -> Option<&StreamInfoBlock> {
        self.metadata_blocks.iter().find_map(|block| match block {
            MetadataBlock::StreamInfo(stream_info) => Some(stream_info),
            _ => None,
        })
    }

    fn vorbis_comment_block(&self) -> Option<&VorbisCommentBlock> {
        self.metadata_blocks.iter().find_map(|block| match block {
            MetadataBlock::VorbisComment(vorbis_comment) => Some(vorbis_comment),
            _ => None,
        })
    }

    fn vorbis_comment_field_value(&self, field_name: &str) -> Option<String> {
        let vorbis = self.vorbis_comment_block()?;
        vorbis.fields.get(field_name).map(|s| s.to_owned())
    }
}

impl Audio for File {
    fn sample_rate(&self) -> f64 {
        self.stream_info_block()
            .map(|info| info.sample_rate as f64)
            .unwrap_or(0f64)
    }

    fn sample_size(&self) -> u16 {
        self.stream_info_block()
            .map(|info| info.bits_per_sample as u16)
            .unwrap_or(0)
    }

    fn channels(&self) -> u16 {
        self.stream_info_block()
            .map(|info| info.number_of_channels as u16)
            .unwrap_or(0)
    }
}

impl ReadTag for File {
    fn album_title(&self) -> Option<String> {
        self.vorbis_comment_field_value("album")
    }

    fn bpm(&self) -> Option<f64> {
        self.vorbis_comment_field_value("bpm")?.parse().ok()
    }

    fn composer(&self) -> Option<String> {
        self.vorbis_comment_field_value("composer")
    }

    fn content_type(&self) -> Option<String> {
        todo!()
    }

    fn copyright_message(&self) -> Option<String> {
        self.vorbis_comment_field_value("copyright")
    }

    fn date(&self) -> Option<String> {
        self.vorbis_comment_field_value("date")
            .or_else(|| self.vorbis_comment_field_value("releasedate"))
    }

    fn playlist_delay(&self) -> Option<String> {
        None
    }

    fn encoded_by(&self) -> Option<String> {
        self.vorbis_comment_field_value("encodedby")
    }

    fn lyricist(&self) -> Option<String> {
        self.vorbis_comment_field_value("lyricist")
    }

    fn file_type(&self) -> Option<String> {
        None
    }

    fn time(&self) -> Option<String> {
        None
    }

    fn content_group_description(&self) -> Option<String> {
        None
    }

    fn title(&self) -> Option<String> {
        self.vorbis_comment_field_value("title")
    }

    fn subtitle(&self) -> Option<String> {
        self.vorbis_comment_field_value("subtitle")
    }

    fn initial_key(&self) -> Option<audio_info::Key> {
        audio_info::Key::parse_key(&self.vorbis_comment_field_value("key")?)
    }

    fn language(&self) -> Option<String> {
        None
    }

    fn length(&self) -> Option<String> {
        None
    }

    fn media_type(&self) -> Option<String> {
        None
    }

    fn original_album(&self) -> Option<String> {
        self.vorbis_comment_field_value("originalalbum")
    }

    fn original_filename(&self) -> Option<String> {
        self.vorbis_comment_field_value("originalfilename")
    }

    fn original_artist(&self) -> Option<String> {
        None
    }

    fn original_release_year(&self) -> Option<u32> {
        let year = match self.vorbis_comment_field_value("originalyear") {
            Some(y) => y,
            None => {
                let mut date = self.vorbis_comment_field_value("originaldate")?;
                date.truncate(4);
                date
            }
        };

        year.parse().ok()
    }

    fn file_owner(&self) -> Option<String> {
        None
    }

    fn lead_artist(&self) -> Option<String> {
        self.vorbis_comment_field_value("artist")
    }

    fn band(&self) -> Option<String> {
        None
    }

    fn conductor(&self) -> Option<String> {
        self.vorbis_comment_field_value("conductor")
    }

    fn modified_by(&self) -> Option<String> {
        None
    }

    fn part_of_set(&self) -> Option<String> {
        None
    }

    fn publisher(&self) -> Option<String> {
        None
    }

    fn track_number(&self) -> Option<audio_info::TrackNumber> {
        let track_string = self.vorbis_comment_field_value("tracknumber")?;
        let total_string = self.vorbis_comment_field_value("totaltracks");

        let track: u32 = track_string.parse().ok()?;
        let total: Option<u32> = total_string.and_then(|t| t.parse().ok());

        Some(audio_info::TrackNumber { track, of: total })
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
        self.vorbis_comment_field_value("isrc")
    }

    fn encoding_settings(&self) -> Option<String> {
        None
    }

    fn year(&self) -> Option<u32> {
        let mut date = self.vorbis_comment_field_value("date")?;
        date.truncate(4);
        date.parse().ok()
    }
}

/// [`Result`] with a [`ParseError`] prefilled.
///
/// [`Result`]: result::Result
/// [`ParseError`]: ParseError
type Result<T> = result::Result<T, ParseError>;

/// Errors that may occur while parsing a FLAC file.
#[derive(Debug)]
pub enum ParseError {
    /// An [`io::Error`].
    ///
    /// [`io::Error`]: io::Error
    Io(io::Error),

    /// Missing a stream info block.
    MissingStreamInfo,

    /// Supposed FLAC file was missing 'fLaC' marker.
    MissingMarker,

    /// Not enough bytes to parse out a valid FLAC file.
    NotEnoughBytes,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Io(error) => write!(f, "could not parse FLAC: IO error: {}", error),
            ParseError::MissingStreamInfo => write!(f, "could not parse FLAC: missing stream info"),
            ParseError::MissingMarker => write!(f, "could not parse FLAC: missing 'fLaC' marker"),
            ParseError::NotEnoughBytes => write!(
                f,
                "could not parse FLAC: not enough bytes to form valid FLAC"
            ),
        }
    }
}

impl Error for ParseError {}

/// A frame of FLAC audio.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Frame {
    header: FrameHeader,
    subframes: Vec<Subframe>,
}

/// Header for [`Frame`]s.
///
/// [`Frame`]: Frame
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FrameHeader {
    variable_size_blocks: bool,

    /// Block size in samples per block.
    block_size: u32,

    sample_rate: Option<u32>,
    channels_description: ChannelsDescription,
    bit_depth: u8,

    /// Fucked little gremlin which is basically the equivilent of a "YOU ARE HERE SIGN" but theres
    /// no map it just points down to the ground below you.
    ///
    /// I'm not gonna spend time parsing this shit any more than I have to in order to find its
    /// length.
    coded_number: [u8; 7],

    crc: u8,
}

/// A FLAC subframe, of which one should exist per channel per frame.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Subframe {
    header: SubframeHeader,
    data: Vec<u8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SubframeHeader {
    /// Wasted bits are the number of padding zero bits in the least significant position of a
    /// sample in order to place it in a greater bit depth container. i.e. a 24-bit sample in a
    /// 32-bit container would have 8 zero bits in the least significant positions of the 32-bit
    /// container.
    wasted_bits: u8,
    subframe_type: SubframeType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SubframeType {
    Constant,
    Verbatim,
    FixedPredictor(u8),
    LinearPredictor(u8),
}

/// Describes the number and use/meaning of audio channels.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ChannelsDescription {
    /// 1 channel - mono.
    Mono,

    /// 2 channels - left, right.
    Stereo,

    /// 3 channels - left, right, center.
    ThreeChannels,

    /// 4 channels - from left, front right, back left, back right.
    FourChannels,

    /// 5 channels - front left, front right, front center, back/surround left, back/surround right.
    FiveChannels,

    /// 6 channels - front left, front right, front center, LFE, back/surround left, back/surround
    /// right.
    FivePointOneSurround,

    /// 7 Channels - 7 channels: front left, front right, front center, LFE, back center, side left,
    /// side right.
    SixPointOneSurround,

    /// 8 channels - front left, front right, front center, LFE, back left, back right, side left,
    /// side right.
    SevenPointOneSurround,

    /// 2 channels - left, right - stored as left-side stereo.
    LeftSideStereo,

    /// 2 channels - left, right - stored as side-right stereo.
    SideRightStereo,

    /// 2 channels - left, right - stored as mid-side stereo.
    MidSideStereo,
}

/// The first 16 bits of a FLAC [`FrameHeader`] if block size is fixed.
///
/// [`FrameHeader`]: FrameHeader
const FRAME_BLOCK_SIZE_FIXED: u16 = FRAME_SYNC_CODE;

/// The first 16 bits of a FLAC [`FrameHeader`] if block size is variable.
///
/// [`FrameHeader`]: FrameHeader
const FRAME_BLOCK_SIZE_VARIABLE: u16 = FRAME_SYNC_CODE | FRAME_STRATEGY_BIT;

/// 15-bit sync code at the beginning of a frame header. This is not a mask, this should be equal to
/// the first 15 bits as they would be left aligned in a 16 bit integer.
const FRAME_SYNC_CODE: u16 = 0b1111_1111_1111_100__0;

/// A bit which, when set, indicates that block sizes are variable in the FLAC. This is the 16th bit
/// which immediately follows the sync code.
const FRAME_STRATEGY_BIT: u16 = 0b0000_0000_0000_000__1;

impl Frame {
    fn from_bytes<'b>(bytes: &'b [u8], stream_info: &StreamInfoBlock) -> Option<(Frame, &'b [u8])> {
        let (header, bytes) = FrameHeader::from_bytes(bytes, stream_info)?;
        let mut bit_buffer = BitBuffer::new(bytes);
        let mut subframes: Vec<Subframe> = Vec::new();

        for _ in 0..header.channels() {
            let subframe = Subframe::from_bits(&mut bit_buffer, &header)?;
            subframes.push(subframe);
        }

        None
    }
}

impl FrameHeader {
    /// Read a [`FrameHeader`] from the given bytes.
    ///
    /// [`FrameHeader`]: FrameHeader
    fn from_bytes<'b>(
        bytes: &'b [u8],
        stream_info: &StreamInfoBlock,
    ) -> Option<(FrameHeader, &'b [u8])> {
        if bytes.len() < 5 {
            return None;
        }

        // The first four bytes are all a static format, but after those first four bytes theres a
        // bit of variable sizing when uncommon block size or sample rates are used, in which case
        // the first bytes of `bit_buffer_tail` should be interpereted differently.
        let mut bit_buffer_top = BitBuffer::new(&bytes[0..4]);
        let mut bit_buffer_tail = BitBuffer::new(&bytes[4..]);

        // Work on the tail first, so that we can pop bits off of it immediately after learning if
        // we are working with an "uncommon" block size or sample rate.
        //
        // Also, this coded number thing is fucked, I mean, who the fuck thinks of this, why is this
        // person too good to just put a 36 bit integer in there? Its up to a 36 bit int inflated to
        // being 7 bytes when encoded, why? why would you do this?
        let coded_number_head_one_bits = bit_buffer_tail.clone().filter(|&i| i > 0).count();
        let coded_number_bytes = match coded_number_head_one_bits {
            0 => 1,
            1 => return None,
            n @ 2..8 => n,
            8.. => return None,
        };

        let mut coded_number: [u8; 7] = [0u8; 7];

        for i in 0..coded_number_bytes {
            let byte = bit_buffer_tail.take8(8)?;
            coded_number[i] = byte;
        }

        let sync_and_strat = bit_buffer_top.take16(16)?;
        let variable_size_blocks = match sync_and_strat {
            FRAME_BLOCK_SIZE_FIXED => false,
            FRAME_BLOCK_SIZE_VARIABLE => true,
            _ => return None,
        };

        let block_size_bits = bit_buffer_top.take8(4)?;
        let block_size = match block_size_bits {
            // Reserved
            0b0000 => return None,

            0b0001 => 192,

            // 144 * (2^v), i.e., 576, 1152, 2304, or 4608
            v @ 0b0010..=0b0101 => 144 * 2u32.pow(v as _),

            // Uncommon block size minus 1, stored as an 8 bit integer.
            0b0110 => {
                let uncomon_size = bit_buffer_tail.take32(8)?;
                uncomon_size + 1
            }

            // Uncommon block size minus 1, stored as an 16 bit integer.
            0b0111 => {
                let uncommon_size = bit_buffer_tail.take32(16)?;
                uncommon_size + 1
            }

            // 2^v, i.e., 256, 512, 1024, 2048, 4096, 8192, 16384, or 32768
            v @ 0b1000..=0b1111 => 2u32.pow(v as _),

            _ => unreachable!("Block size code should be only 4 bits"),
        };

        let sample_rate_bits = bit_buffer_top.take8(4)?;
        let sample_rate = match sample_rate_bits {
            0b0000 => None,
            0b0001 => Some(88_200),
            0b0010 => Some(176_400),
            0b0011 => Some(192_000),
            0b0100 => Some(8_000),
            0b0101 => Some(16_000),
            0b0110 => Some(22_500),
            0b0111 => Some(24_000),
            0b1000 => Some(32_000),
            0b1001 => Some(44_100),
            0b1010 => Some(48_000),
            0b1011 => Some(96_000),

            // Uncommon sample rate in kHz, stored as an 8-bit integer.
            0b1100 => bit_buffer_tail.take8(8).map(|khz| khz as u32 * 1_000),

            // Uncommon sample rate in Hz, stored as a 16-bit integer.
            0b1101 => bit_buffer_tail.take16(16).map(|hertz| hertz as u32),

            // Uncommon sample rate in Hz, divided by 10, stored as a 16-bit integer.
            0b1110 => bit_buffer_tail
                .take16(16)
                .map(|deci_hertz| deci_hertz as u32 * 10),

            _ => unreachable!("Sample rate code should be only 4 bits"),
        };

        let channels_bits = bit_buffer_top.take8(4)?;
        let channels_description = match channels_bits {
            0b0000 => ChannelsDescription::Mono,
            0b0001 => ChannelsDescription::Stereo,
            0b0010 => ChannelsDescription::ThreeChannels,
            0b0011 => ChannelsDescription::FourChannels,
            0b0100 => ChannelsDescription::FiveChannels,
            0b0101 => ChannelsDescription::FivePointOneSurround,
            0b0110 => ChannelsDescription::SixPointOneSurround,
            0b0111 => ChannelsDescription::SevenPointOneSurround,
            0b1000 => ChannelsDescription::LeftSideStereo,
            0b1001 => ChannelsDescription::SideRightStereo,
            0b1010 => ChannelsDescription::MidSideStereo,
            0b1011..=0b1111 => return None,
            _ => unreachable!("Channels bits should only be 4 bits"),
        };

        let bit_depth_bits = bit_buffer_top.take8(3)?;
        let bit_depth = match bit_depth_bits {
            0b000 => stream_info.bits_per_sample, // Bit depth only in stream info.
            0b001 => 8,
            0b010 => 12,
            0b011 => return None, // Reserved
            0b100 => 16,
            0b101 => 20,
            0b110 => 24,
            0b111 => 32,
            _ => unreachable!("Bit depth code should be 3 bits"),
        };

        // Last bit is reserved.
        let _ = bit_buffer_top.take_bit();
        assert!(bit_buffer_top.is_empty());

        let crc = bit_buffer_tail.take8(8)?;

        Some((
            FrameHeader {
                variable_size_blocks,
                block_size,
                sample_rate,
                channels_description,
                bit_depth,
                coded_number,
                crc,
            },
            bit_buffer_top.remaining(),
        ))
    }

    fn channels(&self) -> u8 {
        match self.channels_description {
            ChannelsDescription::Mono => 1,
            ChannelsDescription::Stereo => 2,
            ChannelsDescription::ThreeChannels => 3,
            ChannelsDescription::FourChannels => 4,
            ChannelsDescription::FiveChannels => 5,
            ChannelsDescription::FivePointOneSurround => 6,
            ChannelsDescription::SixPointOneSurround => 7,
            ChannelsDescription::SevenPointOneSurround => 8,
            ChannelsDescription::LeftSideStereo => todo!(),
            ChannelsDescription::SideRightStereo => todo!(),
            ChannelsDescription::MidSideStereo => todo!(),
        }
    }
}

impl Subframe {
    /// Read a [`Subframe`] from the given [`BitBuffer`]. Only the first [`SubframeHeader`] is
    /// guaranteed to be byte-aligned, so the entire section after the [`FrameHeader`] needs to be
    /// read without the assumption of byte alignment.
    ///
    /// [`Subframe`]: Subframe
    /// [`BitBuffer`]: BitBuffer
    fn from_bits(bit_buffer: &mut BitBuffer, frame_header: &FrameHeader) -> Option<Subframe> {
        let header = SubframeHeader::from_bits(bit_buffer)?;

        let data = match header.subframe_type {
            // single sample following the header
            SubframeType::Constant => {
                let mut sample_bits = frame_header.bit_depth - header.wasted_bits;
                let mut sample_data: Vec<u8> = Vec::new();

                while sample_bits >= 8 {
                    sample_data.push(bit_buffer.take8(8)?);
                    sample_bits -= 8;
                }

                if sample_bits > 0 {
                    sample_data.push(bit_buffer.take8(sample_bits as _)? << (7 - sample_bits));
                }

                sample_data
            }

            // multiple samples following the header, number of which is determined by the block
            // size in the frame header
            SubframeType::Verbatim => {
                let sample_bits = frame_header.bit_depth - header.wasted_bits;
                let num_samples = frame_header.block_size / frame_header.channels() as u32;
                let mut sample_data_bits = sample_bits as u32 * num_samples;
                let mut sample_data: Vec<u8> = Vec::new();

                while sample_data_bits >= 8 {
                    sample_data.push(bit_buffer.take8(8)?);
                    sample_data_bits -= 8;
                }

                if sample_data_bits > 0 {
                    sample_data
                        .push(bit_buffer.take8(sample_data_bits as _)? << (7 - sample_data_bits));
                }

                sample_data
            }

            SubframeType::FixedPredictor(predictor_order) => {
                let warmup_sample_bits = frame_header.bit_depth - header.wasted_bits;
                let mut warmup_sample_data_bits = warmup_sample_bits * predictor_order;
                let mut sample_data: Vec<u8> = Vec::new();

                while warmup_sample_data_bits >= 8 {
                    sample_data.push(bit_buffer.take8(8)?);
                    warmup_sample_data_bits -= 8;
                }

                if warmup_sample_data_bits > 0 {
                    sample_data.push(
                        bit_buffer.take8(warmup_sample_data_bits as _)?
                            << (7 - warmup_sample_data_bits),
                    );
                }

                sample_data
            }

            _ => return None,
        };

        None
    }
}

impl SubframeHeader {
    /// Read a [`SubframeHeader`] from the given [`BitBuffer`]. Only the first [`SubframeHeader`] is
    /// guaranteed to be byte-aligned, so the entire section after the [`FrameHeader`] needs to be
    /// read without the assumption of byte alignment.
    ///
    /// [`SubframeHeader`]: SubframeHeader
    /// [`BitBuffer`]: BitBuffer
    fn from_bits(bit_buffer: &mut BitBuffer) -> Option<SubframeHeader> {
        // First bit is always zero.
        if bit_buffer.take_bit()? != 0 {
            return None;
        }

        let subframe_type_bits = bit_buffer.take8(6)?;
        let subframe_type = match subframe_type_bits {
            0b000_000 => SubframeType::Constant,
            0b000_001 => SubframeType::Verbatim,
            0b000_010..=0b000_111 => return None,
            v @ 0b001_000..=0b001_100 => SubframeType::FixedPredictor(v - 8),
            0b001_101..=0b011_111 => return None,
            v @ 0b100_000..=0b111_111 => SubframeType::LinearPredictor(v - 31),
            _ => unreachable!("SubFrame type should be 6 bits"),
        };

        let has_wasted_bits = bit_buffer.take_bit()?;
        let wasted_bits = match has_wasted_bits {
            0 => 0,
            1 => bit_buffer.take_unary()? + 1,
            _ => unreachable!("wasted bits bit should be one bit"),
        };

        Some(SubframeHeader {
            wasted_bits,
            subframe_type,
        })
    }
}

/// Wrapper over the types of metadata block in a FLAC.
#[derive(Debug, PartialEq, Eq, Clone)]
enum MetadataBlock {
    StreamInfo(StreamInfoBlock),
    Padding(PaddingBlock),
    Application(ApplicationBlock),
    SeekTable(SeekTableBlock),
    VorbisComment(VorbisCommentBlock),
    CueSheet(CueSheetBlock),
    Picture(PictureBlock),
    Unknown(TypelessMetadataBlock),
}

/// Contains information relevant to the entire audio stream. This block must be the first metadata
/// block presented, and there must be one and only one instance of this block.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct StreamInfoBlock {
    /// Minimum size of a block, in samples, used in the stream. Excludes the last block. Valid
    /// values are in the 16 to 65,535.
    min_block_samples: u16,

    /// Maximum size of a block, in samples, used in the stream. Valid values are in the 16 to
    /// 65,535.
    max_block_samples: u16,

    /// Minimum size of a frame in bytes used in the stream. Originally a 24 bit integer where 0
    /// indicates unknown.
    min_frame_size: Option<u32>,

    /// Maximum size of a frame in bytes used in the stream. Originally a 24 bit integer where 0
    /// indicates unknown.
    max_frame_size: Option<u32>,

    /// Sample rate in hertz. Originally a 20 bit integer. May not be 0 for audio data. If this
    /// value is 0 it is recommended that the meaning of the encoded data be placed in a
    /// [`VorbisCommentBlock`] or in an [`ApplicationBlock`].
    ///
    /// [`VorbisCommentBlock`]: VorbisCommentBlock
    /// [`ApplicationBlock`]: ApplicationBlock
    sample_rate: u32,

    /// Number of channels. Originally a 3 bit integer. FLAC supports 1 to 8 channels.
    number_of_channels: u8,

    /// Bits per sample. Originally a 5 bit integer. FLAC supports 4 to 32 bits per sample.
    bits_per_sample: u8,

    /// The total number of interchannel samples in the stream. Originally a 36 bit integer where 0
    /// indicated an unknown number.
    number_of_interchannel_samples: u64,

    /// An MD5 checksum for the unencoded audio. The idea here is that a decoder may check its work
    /// against this checksum. This value is originally a 128 bit integer where 0 indicates unknown.
    md5_checksum: Option<u128>,
}

/// A metadata block which holds padding zero bytes. There may be any number of this block type in a
/// FLAC file.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct PaddingBlock {
    /// Size of the padding in bytes. Since once we parse the FLAC we don't actually care about
    /// what's in the padding, we just store its size here so that we can recreate it later.
    size: usize,
}

/// A metadata block for information used by third-party applications.
#[derive(Debug, PartialEq, Eq, Clone)]
struct ApplicationBlock {
    /// A registered application ID. IDs are registered in the IANA "FLAC Application Matadata Block
    /// IDs" registry (see Section 12.2 or RFC9639).
    application_id: u32,

    /// Application data. This should have length equal to the size described in the
    /// [`MetadataBlockHeader`] minus the four bytes already used by the `application_id` field.
    ///
    /// [`MetadataBlockHeader`]: MetadataBlockHeader
    application_data: Vec<u8>,
}

/// There may only be on of this type of matadata block in a FLAC. [`SeekPoint`]s contained in this
/// block should be in order of sample number.
#[derive(Debug, PartialEq, Eq, Clone)]
struct SeekTableBlock(Vec<SeekPoint>);

/// This struct has an implementation of [`Ord`] which will compare the `target_sample_number`
/// field, meaning that any sorted list of [`SeekPoint`]s is sorted by sample number.
///
/// [`Ord`]: Ord
/// [`SeekPoint`]: SeekPoint
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SeekPoint {
    /// Sample number of the first sample in the target frame. [`u64::MAX`] may be used as a
    /// placeholder.
    ///
    /// [`u64::MAX`]: u64::MAX
    target_sample_number: u64,

    /// Offset in bytes from the first byte of the first frame header to the first byte of the
    /// target frame's header.
    frame_header_offset: u64,

    /// Number of samples in the target frame.
    target_number_of_samples: u16,
}

impl SeekPoint {
    /// Whether or not this [`SeekPoint`] is a placeholder.
    ///
    /// [`SeekPoint`]: SeekPoint
    fn is_placeholder(&self) -> bool {
        self.target_sample_number == u64::MAX
    }
}

impl Ord for SeekPoint {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.target_sample_number, &other.target_sample_number)
    }
}

impl PartialOrd for SeekPoint {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ord::cmp(self, other))
    }
}

/// A FLAC may not contain more than one of this type of block.
#[derive(Debug, Clone, PartialEq, Eq)]
struct VorbisCommentBlock {
    vendor_string: String,
    fields: HashMap<String, String>,
}

/// May be used to store the track and index point structure of a Compact Disc Digital Audio (CD-DA)
/// along with its audio, or to simply store location of interest in a FLAC file.
#[derive(Debug, PartialEq, Eq, Clone)]
struct CueSheetBlock {
    /// Media catalog number in ASCII printable characters.
    media_catalog_number: [u8; 128],

    /// Number of lead-in samples.
    lead_in_samples: u64,

    /// Whether of not the cuesheet corresponds to a CD-DA.
    cd_da: bool,

    /// Number of tracks in this cuesheet.
    number_of_tracks: u8,

    /// Tracks. Must have at least one as a cuesheet must have a lead-out track. For CD-DA cuesheets
    /// there may be no more that 100 tracks (99 regular and 1 lead-out). The lead-out track always
    /// comes last. For CD-DA the lead-out track number must be 170, otherwise it must be 255.
    tracks: Vec<CueSheetTrack>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct CueSheetTrack {
    /// Track offset of the first index point in samples, relative to the beginning of the FLAC
    /// audio stream.
    track_offset_samples: u64,

    /// Track number of 0 is not allowed. For CD-DA this number must be in the range 1 to 99, or
    /// 170 for a lead-out track. For non-CD-DA the track number for a lead-out track must be 255.
    /// Track numbers must be unique to the track, and are recommended to start at 1 and count up.
    track_number: u8,

    /// 12-digit alpha-numeric code. A sequence of null characters may be used to indicate the
    /// absence of IRSC.
    track_irsc: [u8; 12],

    /// Whether or not track type is audio or non-audio. Corresponds to the CD-DA Q-Channel control
    /// bit 3.
    is_non_audio: bool,

    /// Whether or not this track should have pre-emphasis. Corresponds to the CD-DA Q-Channel
    /// control bit 5.
    has_pre_emphasis: bool,

    /// The number of track index points.
    track_index_points: u8,

    /// For all but the lead-out [`CueSheetTrack`], the length of this [`Vec`] should be equal to
    /// the `track_index_points` field.
    ///
    /// [`CueSheetTrack`]: CueSheetTrack
    /// [`Vec`]: Vec
    index_points: Vec<CueSheetTrackIndexPoint>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct CueSheetTrackIndexPoint {
    /// Offset in samples, relative to the track offset of the index point. For CD-DA this field's
    /// value must be evenly divisible by 588 samples. This offset is from the beginning of this
    /// track, not from the beginning of the audio data.
    offset_samples: u64,

    /// The track index point number.
    point_number: u8,
}

/// Contains an image that belongs to the audio in some way. There may be any number of this block,
/// but there may only be one of [`PictureType`]s [`PictureType::PngFileIcon`] and
/// [`PictureType::GeneralFileIcon`].
///
/// [`PictureType`]: PictureType
/// [`PictureType::PngFileIcon`]: PictureType::PngFileIcon
/// [`PictureType::GeneralFileIcon`]: PictureType::GeneralFileIcon
#[derive(Debug, PartialEq, Eq, Clone)]
struct PictureBlock {
    picture_type: PictureType,

    /// An ASCII string for the media type as specified by RFC2049, or the text string to specify
    /// that the picture data is a URI to an image rather than picture data itself. This field is
    /// originally preceded by a 32 bit integer for its length in bytes.
    media_type_string: Vec<u8>,

    /// A UTF-8 encoded string describing the image. This field is originally preceded by a 32 bit
    /// integer for its length in bytes.
    description: String,

    /// Width in pixels.
    width: u32,

    /// Height in pixels.
    height: u32,

    /// Bit depth of the picture in bits per pixel.
    color_depth: u32,

    /// For indexed-color pictures like GIFs, this is the number of colors used or [`None`] for a
    /// picture that is not color-indexed. This field is originally a 32 bit integer where 0
    /// indicates a non-indexed picture.
    number_of_colors: Option<u32>,

    /// Binary picture data.
    picture_data: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum PictureType {
    Other,
    PngFileIcon,
    GeneralFileIcon,
    FrontCover,
    BackCover,
    LinerNotesPage,
    MediaLabel,

    /// Lead artist, performer, or soloist.
    LeadArtist,

    /// Artist or performers.
    Artist,

    Conductor,

    /// Band or orchestra.
    Band,

    Composer,

    /// Lyricist or text writer.
    Writer,

    RecordingLocation,
    DuringRecording,
    DuringPerformance,
    VideoScreenCapture,
    BrightColoredFish,
    Illustration,

    /// Band or artist logotype.
    ArtistLogotype,

    /// Publisher or studio logotype.
    StudioLogotype,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct TypelessMetadataBlock {
    header: MetadataBlockHeader,
    data: Vec<u8>,
}

/// Header for metadata block, which holds info about the position, type, and size of the metadata
/// block.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct MetadataBlockHeader(u32);

const METADATA_HEADER_MASK_LAST: u32 = 0b10000000_00000000_00000000_00000000;
const METADATA_HEADER_MASK_TYPE: u32 = 0b01111111_00000000_00000000_00000000;
const METADATA_HEADER_MASK_SIZE: u32 = 0b00000000_11111111_11111111_11111111;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum MetadataBlockType {
    StreamInfo,
    Padding,
    Application,
    SeekTable,
    VorbisComment,
    CueSheet,
    Picture,
    Reserved,
    Forbidden,
}

impl TypelessMetadataBlock {
    fn from_bytes(bytes: &[u8]) -> Option<(TypelessMetadataBlock, &[u8])> {
        let (header, bytes) = MetadataBlockHeader::from_bytes(bytes)?;
        let data = bytes[0..header.size()].to_vec();
        Some((
            TypelessMetadataBlock { header, data },
            &bytes[header.size()..],
        ))
    }

    fn typed(self) -> Option<MetadataBlock> {
        match self.header.block_type() {
            MetadataBlockType::StreamInfo => {
                let mut bit_buffer = BitBuffer::new(&self.data);

                let min_block_samples = bit_buffer.take16(16)?;
                let max_block_samples = bit_buffer.take16(16)?;
                let min_frame_size = match bit_buffer.take32(24)? {
                    0 => None,
                    s => Some(s),
                };
                let max_frame_size = match bit_buffer.take32(24)? {
                    0 => None,
                    s => Some(s),
                };
                let sample_rate = bit_buffer.take32(20)?;
                let number_of_channels = bit_buffer.take8(3)? + 1;
                let bits_per_sample = bit_buffer.take8(5)? + 1;
                let number_of_interchannel_samples = bit_buffer.take64(36)?;
                let md5_checksum = match bit_buffer.take128(128)? {
                    0 => None,
                    md5 => Some(md5),
                };

                Some(MetadataBlock::StreamInfo(StreamInfoBlock {
                    min_block_samples,
                    max_block_samples,
                    min_frame_size,
                    max_frame_size,
                    sample_rate,
                    number_of_channels,
                    bits_per_sample,
                    number_of_interchannel_samples,
                    md5_checksum,
                }))
            }

            MetadataBlockType::Padding => Some(MetadataBlock::Padding(PaddingBlock {
                size: self.data.len(),
            })),

            MetadataBlockType::Application => Some(MetadataBlock::Application(ApplicationBlock {
                application_id: u32::from_be_bytes(*self.data[0..4].as_array()?),
                application_data: self.data[4..].to_vec(),
            })),

            MetadataBlockType::SeekTable => Some(MetadataBlock::Unknown(self)),

            MetadataBlockType::VorbisComment => {
                if self.data.len() < 4 {
                    return None;
                }

                let length = u32::from_le_bytes(*self.data[0..4].as_array()?) as usize;

                if self.data.len() < length + 8 {
                    return None;
                }

                let vendor_string = String::from_utf8(self.data[4..length + 4].to_vec()).ok()?;
                let num_fields = u32::from_le_bytes(*self.data[length + 4..length + 8].as_array()?);

                if num_fields == 0 {
                    return Some(MetadataBlock::VorbisComment(VorbisCommentBlock {
                        vendor_string,
                        fields: HashMap::new(),
                    }));
                }

                let mut remaining_bytes = &self.data[length + 8..];
                let mut fields: HashMap<String, String> = HashMap::new();

                for _ in 0..num_fields {
                    if remaining_bytes.len() < 4 {
                        return None;
                    }

                    let length = u32::from_le_bytes(*remaining_bytes[0..4].as_array()?) as usize;

                    if remaining_bytes.len() < length + 4 {
                        return None;
                    }

                    let string = String::from_utf8(remaining_bytes[4..length + 4].to_vec()).ok()?;
                    remaining_bytes = &remaining_bytes[length + 4..];

                    let (field_name, field_value) = string.split_once('=')?;
                    fields.insert(
                        field_name.to_string().to_lowercase(),
                        field_value.to_string(),
                    );
                }

                Some(MetadataBlock::VorbisComment(VorbisCommentBlock {
                    vendor_string,
                    fields,
                }))
            }

            MetadataBlockType::CueSheet => Some(MetadataBlock::Unknown(self)),
            MetadataBlockType::Picture => Some(MetadataBlock::Unknown(self)),
            MetadataBlockType::Reserved => Some(MetadataBlock::Unknown(self)),
            MetadataBlockType::Forbidden => None,
        }
    }
}

impl MetadataBlockHeader {
    fn from_bytes(bytes: &[u8]) -> Option<(MetadataBlockHeader, &[u8])> {
        if bytes.len() < 4 {
            return None;
        }

        Some((
            MetadataBlockHeader(u32::from_be_bytes(*bytes[0..4].as_array()?)),
            &bytes[4..],
        ))
    }

    fn block_type(&self) -> MetadataBlockType {
        let MetadataBlockHeader(data) = self;
        let type_bits = data & METADATA_HEADER_MASK_TYPE;
        let type_int = type_bits >> 24;

        match type_int {
            0 => MetadataBlockType::StreamInfo,
            1 => MetadataBlockType::Padding,
            2 => MetadataBlockType::Application,
            3 => MetadataBlockType::SeekTable,
            4 => MetadataBlockType::VorbisComment,
            5 => MetadataBlockType::CueSheet,
            6 => MetadataBlockType::Picture,
            7..127 => MetadataBlockType::Reserved,
            127 => MetadataBlockType::Forbidden,
            _ => unreachable!("7-bit integer should not exceed value of 127"),
        }
    }

    fn is_last(&self) -> bool {
        let MetadataBlockHeader(data) = self;
        let is_last = data & METADATA_HEADER_MASK_LAST;
        is_last != 0
    }

    /// Size of the [`MetadataBlock`]'s data in bytes.
    ///
    /// [`MetadataBlock`]: MetadataBlock
    fn size(&self) -> usize {
        let MetadataBlockHeader(data) = self;
        let size_bits = data & METADATA_HEADER_MASK_SIZE;
        size_bits as _
    }
}

struct BlocksIter<'b> {
    bytes: &'b [u8],
    finished: bool,
}

impl<'b> Iterator for BlocksIter<'b> {
    type Item = MetadataBlock;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let (block, bytes) = TypelessMetadataBlock::from_bytes(self.bytes)?;
        block.header.block_type();
        self.bytes = bytes;
        self.finished = block.header.is_last();

        block.typed()
    }
}

/// Wrapper around a [`u8`] slice which allows for any number of bits to be popped off the front of
/// it.
///
/// [`u8`]: u8
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BitBuffer<'i> {
    current_bit: u8,
    current_byte: Option<&'i u8>,
    bytes: &'i [u8],
}

impl<'i> BitBuffer<'i> {
    /// Creates a new [`BitBuffer`] wrapping the given [`u8`] slice .
    ///
    /// [`u8`]: u8
    /// [`BitBuffer`]: BitBuffer
    fn new(bytes: &'i [u8]) -> BitBuffer<'i> {
        BitBuffer {
            current_bit: 0,
            current_byte: bytes.get(0),
            bytes: match bytes.len() {
                1.. => &bytes[1..],
                0 => &[],
            },
        }
    }

    /// Get all remaining whole bytes left in the [`BitBuffer`].
    ///
    /// [`BitBuffer`]: BitBuffer
    fn remaining(&self) -> &'i [u8] {
        self.bytes
    }

    /// True if the [`BitBuffer`] has run through all of its given bits.
    ///
    /// [`BitBuffer`]: BitBuffer
    fn is_empty(&self) -> bool {
        self.current_bit == 8 && self.bytes.is_empty()
    }

    /// Take a unary coded value off the top of the [`BitBuffer`]. e.i. 001 would represent 2, and
    /// 000001 would represent 5.
    ///
    /// [`BitBuffer`]: BitBuffer
    fn take_unary(&mut self) -> Option<u8> {
        let mut value = 0;

        while self.take_bit()? == 0 {
            value += 1;
        }

        Some(value)
    }

    /// Take the given number of bits from the [`BitBuffer`], and return them in a left alligned
    /// integer, so taking 4 bits all set to 1 would result in the [`u8`] `0b0000_1111`.
    ///
    /// [`BitBuffer`]: BitBuffer
    /// [`u8`]: u8
    fn take8(&mut self, bits: u8) -> Option<u8> {
        match bits {
            0 => None,
            1 => self.take_bit(),
            2.. => Some((self.take_bit()? << (bits - 1)) | self.take8(bits - 1)?),
        }
    }

    /// Like [`BitBuffer::take8`] but can take up to 16 bits and returns a [`u16`].
    ///
    /// [`BitBuffer::take`]: BitBuffer::take
    /// [`u16`]: u16
    fn take16(&mut self, bits: u8) -> Option<u16> {
        match bits {
            0 => None,
            1 => Some(self.take_bit()? as _),
            2.. => Some(((self.take_bit()? as u16) << (bits - 1)) | self.take16(bits - 1)?),
        }
    }

    /// Like [`BitBuffer::take8`] but can take up to 32 bits and returns a [`u32`].
    ///
    /// [`BitBuffer::take`]: BitBuffer::take
    /// [`u32`]: u32
    fn take32(&mut self, bits: u8) -> Option<u32> {
        match bits {
            0 => None,
            1 => Some(self.take_bit()? as _),
            2.. => Some(((self.take_bit()? as u32) << (bits - 1)) | self.take32(bits - 1)?),
        }
    }

    /// Like [`BitBuffer::take8`] but can take up to 64 bits and returns a [`u64`].
    ///
    /// [`BitBuffer::take`]: BitBuffer::take
    /// [`u64`]: u64
    fn take64(&mut self, bits: u8) -> Option<u64> {
        match bits {
            0 => None,
            1 => Some(self.take_bit()? as _),
            2.. => Some(((self.take_bit()? as u64) << (bits - 1)) | self.take64(bits - 1)?),
        }
    }

    /// Like [`BitBuffer::take8`] but can take up to 128 bits and returns a [`u128`].
    ///
    /// [`BitBuffer::take`]: BitBuffer::take
    /// [`u128`]: u128
    fn take128(&mut self, bits: u8) -> Option<u128> {
        match bits {
            0 => None,
            1 => Some(self.take_bit()? as _),
            2.. => Some(((self.take_bit()? as u128) << (bits - 1)) | self.take128(bits - 1)?),
        }
    }

    /// Take a single bit and returns it in the right-most bit of a [`u8`].
    ///
    /// [`u8`]: u8
    fn take_bit(&mut self) -> Option<u8> {
        match self.current_bit {
            8 => {
                self.current_byte = self.bytes.get(0);
                self.current_bit = 0;
                self.bytes = match self.bytes.len() {
                    1.. => &self.bytes[1..],
                    0 => &[],
                };

                self.take_bit()
            }

            bit @ 0..8 => {
                let masked = self.current_byte? & (0b1000_0000 >> bit);
                self.current_bit += 1;
                Some(masked >> (7 - bit))
            }

            _ => None,
        }
    }
}

impl<'i> Iterator for BitBuffer<'i> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        self.take_bit()
    }
}

#[cfg(test)]
mod tests {
    use crate::flac::BitBuffer;

    #[test]
    fn bit_buffer() {
        let bytes = [0b0000_1111, 0b1100_1100, 0b1100_1100];

        let mut bit_buffer = BitBuffer::new(&bytes);

        assert_eq!(bit_buffer.take8(2), Some(0b00));
        assert_eq!(bit_buffer.take8(4), Some(0b0011));
        assert_eq!(bit_buffer.take8(2), Some(0b11));
        assert_eq!(bit_buffer.take16(16), Some(0b1100_1100_1100_1100));
        assert_eq!(bit_buffer.take8(1), None);
    }
}
