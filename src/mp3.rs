use crate::{
    audio_info::{self, Audio, ReadTag, WriteTag},
    id3v1, id3v2,
};
use std::{
    error::Error,
    fmt::Display,
    fs,
    io::{self, Write},
    ops::Range,
    path::Path,
    result,
};

/// An MP3 file.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct File {
    /// The [`Frame`]s which make up the MP3's sound.
    pub frames: Vec<Frame>,

    /// Either an [`id3v1::Tag`] or [`id3v2::Tag`] containing information about the track.
    ///
    /// [`id3v1::Tag`]: id3v1::Tag
    /// [`id3v2::Tag`]: id3v2::Tag
    pub tag: Tag,
}

audio_info::impl_read_tag!(File, File::tag);
audio_info::impl_write_tag!(File, File::tag_mut);

impl File {
    /// Open and parse an MP3 file for reading track info.
    pub fn read_from(path: impl AsRef<Path>) -> Result<File> {
        let bytes = fs::read(path).map_err(|e| ParseError::Io(e))?;
        File::from_bytes(&bytes)
    }

    /// Serialize this [`mp3::File`] into bytes, then write them to a file at the given
    /// [`AsRef<Path>`].
    ///
    /// [`mp3::File`]: File
    /// [`AsRef<Path>`]: AsRef<Path>
    pub fn write_to(self, path: impl AsRef<Path>) -> Result<()> {
        let mut file = fs::File::create(path).map_err(|io_err| ParseError::Io(io_err))?;
        let bytes = self.to_bytes();
        file.write_all(&bytes)
            .map_err(|io_err| ParseError::Io(io_err))?;

        Ok(())
    }

    /// Serialize this [`mp3::File`] into bytes.
    ///
    /// [`mp3::File`]: File
    fn to_bytes(self) -> Vec<u8> {
        match self.tag {
            // ID3v2 is expected to preffix the MP3.
            Tag::Id3v2(tag) => {
                let mut bytes: Vec<u8> = tag.to_bytes();
                bytes.append(&mut self.frames.into_iter().flat_map(|f| f.to_bytes()).collect());
                bytes
            }

            Tag::Id3v1(tag) => {
                let mut bytes: Vec<u8> =
                    self.frames.into_iter().flat_map(|f| f.to_bytes()).collect();
                bytes.append(&mut tag.to_bytes().to_vec());
                bytes
            }
        }
    }

    /// Parse an [`mp3::File`] from bytes.
    ///
    /// [`mp3::File`]: File
    fn from_bytes(bytes: &[u8]) -> Result<File> {
        match id3v2::parse_tag(&bytes) {
            // Treat the file as having an ID3v2 tag.
            Ok((v2_tag, remaining_bytes)) => {
                let frames = FramesIter { remaining_bytes }.collect();
                let tag = Tag::Id3v2(v2_tag);
                Ok(File { frames, tag })
            }

            // The starting ID for an ID3v2 tag was not present, meaning that one does not exist, so
            // try to treat the file as having an ID3v1 tag.
            Err(id3v2::ParseError::BadId) => {
                let mut frames: Vec<Frame> = Vec::new();
                let mut frames_iter = FramesIter {
                    remaining_bytes: &bytes,
                };

                while let Some(frame) = frames_iter.next() {
                    frames.push(frame);
                }

                let (v1_tag, _) = id3v1::parse_tag(frames_iter.remaining_bytes)
                    .ok_or(ParseError::ExpectedId3v1)?;
                let tag = Tag::Id3v1(v1_tag);

                Ok(File { frames, tag })
            }

            Err(e) => {
                return Err(ParseError::Id3v2ParseError(e));
            }
        }
    }

    /// Gets a [`ReadTag`], meant for implementing [`ReadTag`] with macros.
    ///
    /// [`ReadTag`]: ReadTag
    fn tag(&self) -> Option<&dyn ReadTag> {
        Some(self.tag.tag())
    }

    /// Gets a [`WriteTag`], meant for implementing [`WriteTag`] with macros.
    ///
    /// [`WriteTag`]: WriteTag
    fn tag_mut(&mut self) -> Option<&mut dyn WriteTag> {
        Some(self.tag.tag_mut())
    }
}

impl Audio for File {
    fn sample_rate(&self) -> f64 {
        self.frames
            .first()
            .map(|frame| frame.header.sample_rate().unwrap_or(0))
            .unwrap_or(0) as _
    }

    fn sample_size(&self) -> u16 {
        let bit_rate = self
            .frames
            .first()
            .map(|frame| match frame.header.bit_rate() {
                Some(Bitrate::Rate(rate)) => rate,
                Some(Bitrate::Free) => 0,
                None => 0,
            })
            .unwrap_or(0);
        let sample_rate = self.sample_rate();

        if sample_rate == 0f64 {
            return 0;
        }

        bit_rate as u16 / sample_rate as u16
    }

    fn channels(&self) -> u16 {
        self.frames
            .first()
            .map(|frame| match frame.header.channel_mode() {
                Some(
                    ChannelMode::DualChannel
                    | ChannelMode::Stereo
                    | ChannelMode::JointStereoL1L2 { .. }
                    | ChannelMode::JointStereoL3 { .. },
                ) => 2,
                Some(ChannelMode::SingleChannel) => 1,
                None => 0,
            })
            .unwrap_or(0)
    }
}

/// An ID3 tag, either [`id3v1::Tag`] or [`id3v2::Tag`]>
///
/// [`id3v1::Tag`]: id3v1::Tag
/// [`id3v2::Tag`]: id3v2::Tag
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Tag {
    /// ID3 version 2 tag.
    Id3v2(id3v2::Tag),

    /// ID3 version 1 tag.
    Id3v1(id3v1::Tag),
}

impl Tag {
    /// Get a reference to a [`ReadTag`] instance.
    ///
    /// [`ReadTag`]: ReadTag
    fn tag(&self) -> &dyn ReadTag {
        match self {
            Tag::Id3v1(tag) => tag,
            Tag::Id3v2(tag) => tag,
        }
    }

    /// Get a mutable reference to a [`WriteTag`] instance.
    ///
    /// [`WriteTag`]: WriteTag
    fn tag_mut(&mut self) -> &mut dyn WriteTag {
        match self {
            Tag::Id3v1(tag) => tag,
            Tag::Id3v2(tag) => tag,
        }
    }
}

/// A single MP3 frame.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Frame {
    /// The [`Frame`]'s header, containing bit rate and sample information among other things.
    ///
    /// [`Frame`]: Frame
    pub header: FrameHeader,

    /// The [`Frame`]'s raw frame data. This is what comes after the [`FrameHeader`] and is unparsed
    /// here. The sounds data is not decrompressed here.
    ///
    /// [`Frame`]: Frame
    /// [`FrameHeader`]: FrameHeader
    pub data: Vec<u8>,
}

/// An MP3 frame header. MP3 (MPEG-1 Audio Layer III or MPEG-2 Audio Layer III) does not have file
/// headers, instead it is made up or largely independant frames, each with their own 32-bit
/// headers. The frames are not _entirely_ independant in the case of variable bit rate.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct FrameHeader(u32);

impl Frame {
    /// Parse a [`Frame`] from the given [`slice`] of bytes, returning the [`Frame`] and the left
    /// over bytes in a tuple. Gives [`None`] if not enough bytes are present or if the
    /// [`FrameHeader`] cannot be validly parsed for a length.
    ///
    /// [`Frame`]: Frame
    /// [`FrameHeader`]: FrameHeader
    /// [`slice`]: std::slice
    /// [`None`]: Option::None
    fn from_bytes(bytes: &[u8]) -> Option<(Frame, &[u8])> {
        let (header, remaining_bytes) = FrameHeader::from_bytes(bytes)?;
        let frame_length = header.frame_length()? as usize;

        if frame_length > remaining_bytes.len() {
            return None;
        }

        let data = remaining_bytes[..frame_length].to_vec();
        let remaining_bytes = &remaining_bytes[frame_length..];
        Some((Frame { header, data }, remaining_bytes))
    }

    /// Serialize this [`Frame`] into bytes.
    ///
    /// [`Frame`]: Frame
    fn to_bytes(mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.header.to_bytes().to_vec());
        bytes.append(&mut self.data);
        bytes
    }
}

/// An [`Iterator`] over [`Frame`]s from an MP3 file.
///
/// [`Iterator`]: Iterator
/// [`Frame`]: Frame
struct FramesIter<'b> {
    /// The remaining bytes after the last call to [`next`].
    ///
    /// [`next`]: Iterator::next
    remaining_bytes: &'b [u8],
}

impl<'d> Iterator for FramesIter<'d> {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        let (frame, remaining_bytes) = Frame::from_bytes(self.remaining_bytes)?;
        self.remaining_bytes = remaining_bytes;
        Some(frame)
    }
}

/// Alias for a [`Result`] with the error pre-filled as a [`ParseError`].
///
/// [`Result`]: result::Result
/// [`ParseError`]: ParseError
pub type Result<T> = result::Result<T, ParseError>;

/// Errors which may occur when parsing an MP3.
#[derive(Debug)]
pub enum ParseError {
    /// IO errors.
    Io(io::Error),

    /// Expected an ID3v1 tag.
    ExpectedId3v1,

    /// An error parsing an ID3v2 tag.
    Id3v2ParseError(id3v2::ParseError),
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Io(error) => write!(f, "io error while parsing MP3: {}", error),
            ParseError::ExpectedId3v1 => write!(f, "expected ID3v1 tag"),
            ParseError::Id3v2ParseError(parse_error) => {
                write!(f, "ID3v2 parse error while parsing MP3: {}", parse_error)
            }
        }
    }
}

/// MPEG version.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Version {
    /// MPEG version 2.5.
    Mpeg2_5 = 0b00,

    /// MPEG version 2.
    Mpeg2 = 0b10,

    /// MPEG version 1.
    Mpeg1 = 0b11,
}

/// MPEG layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Layer {
    /// MPEG Layer I.
    LayerI = 0b11,

    /// MPEG Layer II.
    LayerII = 0b10,

    /// MPEG Layer III.
    LayerIII = 0b01,
}

/// Bitrate, either an actuall rate or "free", which indicates that the application is expected to
/// decide the bitrate somehow. This is supposed to be used for bitrates that are otherwise
/// unsupported, however it does require special application support.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Bitrate {
    /// Indicates application-determined bitrate, intended for making unsupported bitrates possible.
    Free,

    /// A bitrate.
    Rate(u32),
}

/// MP3 channel mode.
#[derive(PartialEq, Eq, Clone, Debug)]
enum ChannelMode {
    Stereo,
    JointStereoL1L2 {
        intensity_stereo_bands: Range<u32>,
    },
    JointStereoL3 {
        intensity_stereo: bool,
        ms_stereo: bool,
    },
    DualChannel,
    SingleChannel,
}

const FRAME_SYNC_MASK: u32 = 0b_11111111_11100000_00000000_00000000;
const FRAME_SYNC_SHIFT: u32 = 21;

const VERSION_MASK: u32 = 0b_00000000_00011000_00000000_00000000;
const VERSION_SHIFT: u32 = 19;

const LAYER_DESCRIPTION_MASK: u32 = 0b_00000000_00000110_00000000_00000000;
const LAYER_DESCRIPTION_SHIFT: u32 = 17;

const PROTECTION_BIT_MASK: u32 = 0b_00000000_00000001_00000000_00000000;
const PROTECTION_BIT_SHIFT: u32 = 16;

const BITRATE_MASK: u32 = 0b_00000000_00000000_11110000_00000000;
const BITRATE_SHIFT: u32 = 12;

const SAMPLING_RATE_MASK: u32 = 0b_00000000_00000000_00001100_00000000;
const SAMPLING_RATE_SHIFT: u32 = 10;

const PADDING_BIT_MASK: u32 = 0b_00000000_00000000_00000010_00000000;
const PADDING_BIT_SHIFT: u32 = 9;

const PRIVATE_BIT_MASK: u32 = 0b_00000000_00000000_00000001_00000000;
const PRIVATE_BIT_SHIFT: u32 = 8;

const CHANNEL_MODE_MASK: u32 = 0b_00000000_00000000_00000000_11000000;
const CHANNEL_MODE_SHIFT: u32 = 6;

const MODE_EXTENSION_MASK: u32 = 0b_00000000_00000000_00000000_00110000;
const MODE_EXTENSION_SHIFT: u32 = 4;

const COPYRIGHT_MASK: u32 = 0b_00000000_00000000_00000000_00001000;
const COPYRIGHT_SHIFT: u32 = 3;

const ORIGINAL_MASK: u32 = 0b_00000000_00000000_00000000_00000100;
const ORIGINAL_SHIFT: u32 = 2;

const EMPHASIS_MASK: u32 = 0b_00000000_00000000_00000000_00000011;
const EMPHASIS_SHIFT: u32 = 0;

impl FrameHeader {
    fn new(
        version: Version,
        layer: Layer,
        bitrate_bits: u8,
        sample_rate_bits: u8,
        padded: bool,
        private: bool,
        channel_mode: ChannelMode,
        copyrighted: bool,
        original: bool,
    ) -> FrameHeader {
        let channel_mode_bits: u32 = match channel_mode {
            ChannelMode::Stereo => 0b0000,
            ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: Range { start: 4, end: 31 },
            } => 0b0100,
            ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: Range { start: 8, end: 31 },
            } => 0b0101,
            ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: Range { start: 12, end: 31 },
            } => 0b0110,
            ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: Range { start: 16, end: 31 },
            } => 0b0111,
            ChannelMode::JointStereoL3 {
                intensity_stereo: true,
                ms_stereo: true,
            } => 0b0111,
            ChannelMode::JointStereoL3 {
                intensity_stereo: true,
                ms_stereo: false,
            } => 0b0101,
            ChannelMode::JointStereoL3 {
                intensity_stereo: false,
                ms_stereo: true,
            } => 0b0110,
            ChannelMode::JointStereoL3 {
                intensity_stereo: false,
                ms_stereo: false,
            } => 0b0100,
            ChannelMode::DualChannel => 0b1000,
            ChannelMode::SingleChannel => 0b1100,
            _ => unreachable!(),
        };

        FrameHeader(
            0b_11111111_11100000_00000000_00000000
                | ((version as u32) << 19)
                | ((layer as u32) << 17)
                | ((bitrate_bits as u32) << 12)
                | ((sample_rate_bits as u32) << 10)
                | ((padded as u32) << 9)
                | ((private as u32) << 8)
                | (channel_mode_bits << 4)
                | ((copyrighted as u32) << 3)
                | ((original as u32) << 2),
        )
    }
    /// Parse a [`FrameHeader`] from the given [`slice`] of bytes. Returns the parsed
    /// [`FrameHeader`] as well as the left over bytes after parsing.
    ///
    /// [`FrameHeader`]: FrameHeader
    /// [`slice`]: std::slice
    fn from_bytes(bytes: &[u8]) -> Option<(FrameHeader, &[u8])> {
        if bytes.len() < size_of::<FrameHeader>() {
            return None;
        }

        let header = u32::from_be_bytes(*bytes[..4].as_array()?);
        let remaining = &bytes[4..];

        if header & FRAME_SYNC_MASK != FRAME_SYNC_MASK {
            return None;
        }

        Some((FrameHeader(header), remaining))
    }

    /// Serialize this [`FrameHeader`] into bytes.
    ///
    /// [`FrameHeader`]: FrameHeader
    fn to_bytes(self) -> [u8; size_of::<FrameHeader>()] {
        self.0.to_be_bytes()
    }

    /// Gives the length of the [`Frame`] in bytes (not the typical 4 byte slots). This is the
    /// length when compressed.
    ///
    /// [`Frame`]: Frame
    fn frame_length(&self) -> Option<u32> {
        let bit_rate = match self.bit_rate()? {
            Bitrate::Free => return None,
            Bitrate::Rate(r) => r,
        };

        Some(match self.layer()? {
            Layer::LayerI => (12 * bit_rate / self.sample_rate()? + self.padding()?) * 4,
            Layer::LayerII | Layer::LayerIII => {
                144 * bit_rate / self.sample_rate()? + self.padding()?
            }
        })
    }

    /// The number of samples in the [`Frame`], this is a constant dependant on the MPEG layer. For
    /// Layer I it is 384 samples, and for Layers II and III it is 1152 samples.
    fn frame_size(&self) -> Option<u32> {
        match self.layer()? {
            Layer::LayerI => Some(384),
            Layer::LayerII | Layer::LayerIII => Some(1152),
        }
    }

    /// Get the [`Frame`]'s MPEG version.
    ///
    /// [`Frame`]: Frame
    fn mpeg_version(&self) -> Option<Version> {
        let version_bits = (self.0 & VERSION_MASK) >> VERSION_SHIFT;

        match version_bits {
            0b11 => Some(Version::Mpeg1),
            0b10 => Some(Version::Mpeg2),
            0b00 => Some(Version::Mpeg2_5),
            _ => None,
        }
    }

    /// Get the [`Frame`]'s MPEG layer.
    ///
    /// [`Frame`]: Frame
    fn layer(&self) -> Option<Layer> {
        let layer_bits = (self.0 & LAYER_DESCRIPTION_MASK) >> LAYER_DESCRIPTION_SHIFT;

        match layer_bits {
            0b11 => Some(Layer::LayerI),
            0b10 => Some(Layer::LayerII),
            0b01 => Some(Layer::LayerIII),
            _ => None,
        }
    }

    /// Gets the bit rate, in bits per second, of the [`Frame`].
    ///
    /// [`Frame`]: Frame
    fn bit_rate(&self) -> Option<Bitrate> {
        use Bitrate::{Free, Rate};
        use ChannelMode::{DualChannel, JointStereoL3, SingleChannel, Stereo};
        use Layer::{LayerI, LayerII, LayerIII};
        use Version::{Mpeg1, Mpeg2, Mpeg2_5};

        let bitrate_bits = (self.0 & BITRATE_MASK) >> BITRATE_SHIFT;

        match (
            bitrate_bits,
            self.mpeg_version()?,
            self.layer()?,
            self.channel_mode()?,
        ) {
            (0b0000, _, _, _) => Some(Free),

            (0b0001, Mpeg1, LayerII, SingleChannel) => Some(Rate(32_000)),
            (0b0001, Mpeg1, _, _) => Some(Rate(32_000)),
            (0b0001, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(8_000)),

            (0b0010, Mpeg1, LayerI, _) => Some(Rate(64_000)),
            (0b0010, Mpeg1, LayerII, SingleChannel) => Some(Rate(48_000)),
            (0b0010, Mpeg1, LayerIII, _) => Some(Rate(40_000)),
            (0b0010, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(48_000)),
            (0b0010, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(16_000)),

            (0b0011, Mpeg1, LayerI, _) => Some(Rate(96_000)),
            (0b0011, Mpeg1, LayerII, SingleChannel) => Some(Rate(56_000)),
            (0b0011, Mpeg1, LayerIII, _) => Some(Rate(48_000)),
            (0b0011, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(56_000)),
            (0b0011, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(24_000)),

            (0b0100, Mpeg1, LayerI, _) => Some(Rate(128_000)),
            (0b0100, Mpeg1, LayerII, _) => Some(Rate(64_000)),
            (0b0100, Mpeg1, LayerIII, _) => Some(Rate(56_000)),
            (0b0100, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(64_000)),
            (0b0100, Mpeg2 | Mpeg2_5, LayerII, SingleChannel) => Some(Rate(32_000)),
            (0b0100, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(32_000)),

            (0b0101, Mpeg1, LayerI, _) => Some(Rate(160_000)),
            (0b0101, Mpeg1, LayerII, SingleChannel) => Some(Rate(80_000)),
            (0b0101, Mpeg1, LayerIII, _) => Some(Rate(64_000)),
            (0b0101, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(80_000)),
            (0b0101, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(40_000)),

            (0b0110, Mpeg1, LayerI, _) => Some(Rate(192_000)),
            (0b0110, Mpeg1, LayerII, _) => Some(Rate(96_000)),
            (0b0110, Mpeg1, LayerIII, _) => Some(Rate(80_000)),
            (0b0110, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(96_000)),
            (0b0110, Mpeg2 | Mpeg2_5, LayerII, SingleChannel) => Some(Rate(48_000)),
            (0b0110, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(48_000)),

            (0b0111, Mpeg1, LayerI, _) => Some(Rate(224_000)),
            (0b0111, Mpeg1, LayerII, _) => Some(Rate(112_000)),
            (0b0111, Mpeg1, LayerIII, _) => Some(Rate(96_000)),
            (0b0111, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(112_000)),
            (0b0111, Mpeg2 | Mpeg2_5, LayerII, SingleChannel) => Some(Rate(56_000)),
            (0b0111, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(56_000)),

            (0b1000, Mpeg1, LayerI, _) => Some(Rate(256_000)),
            (0b1000, Mpeg1, LayerII, _) => Some(Rate(128_000)),
            (0b1000, Mpeg1, LayerIII, _) => Some(Rate(112_000)),
            (0b1000, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(128_000)),
            (0b1000, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(64_000)),

            (0b1001, Mpeg1, LayerI, _) => Some(Rate(288_000)),
            (0b1001, Mpeg1, LayerII, _) => Some(Rate(160_000)),
            (0b1001, Mpeg1, LayerIII, _) => Some(Rate(128_000)),
            (0b1001, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(144_000)),
            (0b1001, Mpeg2 | Mpeg2_5, LayerII, SingleChannel) => Some(Rate(80_000)),
            (0b1001, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(80_000)),

            (0b1010, Mpeg1, LayerI, _) => Some(Rate(320_000)),
            (0b1010, Mpeg1, LayerII, _) => Some(Rate(192_000)),
            (0b1010, Mpeg1, LayerIII, _) => Some(Rate(160_000)),
            (0b1010, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(160_000)),
            (0b1010, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(96_000)),

            (0b1011, Mpeg1, LayerI, _) => Some(Rate(352_000)),
            (
                0b1011,
                Mpeg1,
                LayerII,
                Stereo
                | JointStereoL3 {
                    intensity_stereo: true,
                    ..
                }
                | DualChannel,
            ) => Some(Rate(224_000)),
            (0b1011, Mpeg1, LayerIII, _) => Some(Rate(192_000)),
            (0b1011, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(176_000)),
            (0b1011, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(112_000)),

            (0b1100, Mpeg1, LayerI, _) => Some(Rate(384_000)),
            (
                0b1100,
                Mpeg1,
                LayerII,
                Stereo
                | JointStereoL3 {
                    intensity_stereo: true,
                    ..
                }
                | DualChannel,
            ) => Some(Rate(256_000)),
            (0b1100, Mpeg1, LayerIII, _) => Some(Rate(224_000)),
            (0b1100, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(192_000)),
            (0b1100, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(128_000)),

            (0b1101, Mpeg1, LayerI, _) => Some(Rate(416_000)),
            (
                0b1101,
                Mpeg1,
                LayerII,
                Stereo
                | JointStereoL3 {
                    intensity_stereo: true,
                    ..
                }
                | DualChannel,
            ) => Some(Rate(320_000)),
            (0b1101, Mpeg1, LayerIII, _) => Some(Rate(256_000)),
            (0b1101, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(224_000)),
            (0b1101, Mpeg2 | Mpeg2_5, LayerIII, _) => Some(Rate(144_000)),

            (0b1110, Mpeg1, LayerI, _) => Some(Rate(448_000)),
            (
                0b1110,
                Mpeg1,
                LayerII,
                Stereo
                | JointStereoL3 {
                    intensity_stereo: true,
                    ..
                }
                | DualChannel,
            ) => Some(Rate(384_000)),
            (0b1110, Mpeg1, LayerIII, _) => Some(Rate(320_000)),
            (0b1110, Mpeg2 | Mpeg2_5, LayerI, _) => Some(Rate(256_000)),
            (0b1110, Mpeg2 | Mpeg2_5, LayerII | LayerIII, _) => Some(Rate(160_000)),

            _ => None,
        }
    }

    /// Get the channel mode of the [`Frame`].
    ///
    /// [`Frame`]: Frame
    fn channel_mode(&self) -> Option<ChannelMode> {
        let channel_mode_bits = (self.0 & CHANNEL_MODE_MASK) >> CHANNEL_MODE_SHIFT;

        match channel_mode_bits {
            0b00 => Some(ChannelMode::Stereo),
            0b01 => self.mode_extension(),
            0b10 => Some(ChannelMode::DualChannel),
            0b11 => Some(ChannelMode::SingleChannel),
            _ => unreachable!(),
        }
    }

    /// Assuming that the channel mode is [`ChannelMode::JointStereo`], get the extra extended
    /// channel mode information.
    ///
    /// [`ChannelMode::JoinStereo`]: ChannelMode::JoinStereo
    fn mode_extension(&self) -> Option<ChannelMode> {
        let extension_bits = (self.0 & MODE_EXTENSION_MASK) >> MODE_EXTENSION_SHIFT;

        match (extension_bits, self.layer()?) {
            (0b00, Layer::LayerIII) => Some(ChannelMode::JointStereoL3 {
                intensity_stereo: false,
                ms_stereo: false,
            }),

            (0b01, Layer::LayerIII) => Some(ChannelMode::JointStereoL3 {
                intensity_stereo: true,
                ms_stereo: false,
            }),

            (0b10, Layer::LayerIII) => Some(ChannelMode::JointStereoL3 {
                intensity_stereo: false,
                ms_stereo: true,
            }),

            (0b11, Layer::LayerIII) => Some(ChannelMode::JointStereoL3 {
                intensity_stereo: true,
                ms_stereo: true,
            }),

            (0b00, Layer::LayerI | Layer::LayerII) => Some(ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: 4..31,
            }),

            (0b01, Layer::LayerI | Layer::LayerII) => Some(ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: 8..31,
            }),

            (0b10, Layer::LayerI | Layer::LayerII) => Some(ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: 12..31,
            }),

            (0b11, Layer::LayerI | Layer::LayerII) => Some(ChannelMode::JointStereoL1L2 {
                intensity_stereo_bands: 16..31,
            }),

            _ => unreachable!(),
        }
    }

    /// The sample rate, in hertz, of the [`Frame`].
    ///
    /// [`Frame`]: Frame
    fn sample_rate(&self) -> Option<u32> {
        let sample_rate_bits = (self.0 & SAMPLING_RATE_MASK) >> SAMPLING_RATE_SHIFT;

        match (sample_rate_bits, self.mpeg_version()?) {
            (0b00, Version::Mpeg1) => Some(44_100),
            (0b00, Version::Mpeg2) => Some(22_050),
            (0b00, Version::Mpeg2_5) => Some(11_025),

            (0b01, Version::Mpeg1) => Some(48_000),
            (0b01, Version::Mpeg2) => Some(24_000),
            (0b01, Version::Mpeg2_5) => Some(12_000),

            (0b10, Version::Mpeg1) => Some(32_000),
            (0b10, Version::Mpeg2) => Some(16_000),
            (0b10, Version::Mpeg2_5) => Some(8_000),

            _ => None,
        }
    }

    /// The padding, in bytes, of the given [`Frame`].
    ///
    /// [`Frame`]: Frame
    fn padding(&self) -> Option<u32> {
        let padding_bit = (self.0 & PADDING_BIT_MASK) >> PADDING_BIT_SHIFT;

        if padding_bit > 0 {
            match self.layer()? {
                Layer::LayerI => Some(4),
                Layer::LayerII | Layer::LayerIII => Some(1),
            }
        } else {
            Some(0)
        }
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn to_bytes() {
//         let header = FrameHeader::new(
//             Version::Mpeg2_5,
//             Layer::LayerIII,
//             0b1110,
//             0b00,
//             false,
//             false,
//             ChannelMode::SingleChannel,
//             false,
//             true,
//         );

//         let tag = id3v2::Tag::new(vec![
//             (AudioTag::LeadArtist, "John Doe"),
//             (AudioTag::Title, "Sick Beat"),
//             (AudioTag::AlbumTitle, "John Doe's Mixtape"),
//         ]);

//         let mp3 = File {
//             frames: vec![Frame {
//                 header,
//                 data: vec![0; header.frame_length().unwrap() as usize],
//             }],
//             tag: Tag::Id3v2(tag.clone()),
//         };

//         let mut bytes: Vec<u8> = tag.to_bytes();
//         bytes.append(&mut vec![
//             0b_1111_1111,
//             0b_1110_0010,
//             0b_1110_0000,
//             0b_1100_0100,
//         ]);
//         bytes.append(&mut vec![0; header.frame_length().unwrap() as usize]);

//         assert_eq!(mp3.to_bytes(), bytes);
//     }

//     #[test]
//     fn from_bytes() {
//         let header = FrameHeader::new(
//             Version::Mpeg2_5,
//             Layer::LayerIII,
//             0b1110,
//             0b00,
//             false,
//             false,
//             ChannelMode::SingleChannel,
//             false,
//             true,
//         );

//         let tag = id3v2::Tag::new(vec![
//             (AudioTag::LeadArtist, "John Doe"),
//             (AudioTag::Title, "Sick Beat"),
//             (AudioTag::AlbumTitle, "John Doe's Mixtape"),
//         ]);

//         let mut bytes: Vec<u8> = tag.clone().to_bytes();
//         bytes.append(&mut vec![
//             0b_1111_1111,
//             0b_1110_0010,
//             0b_1110_0000,
//             0b_1100_0100,
//         ]);
//         bytes.append(&mut vec![0; header.frame_length().unwrap() as usize]);

//         let mp3 = File {
//             frames: vec![Frame {
//                 header,
//                 data: vec![0; header.frame_length().unwrap() as usize],
//             }],
//             tag: Tag::Id3v2(tag),
//         };

//         assert_eq!(File::from_bytes(&bytes).unwrap(), mp3);
//     }
// }
