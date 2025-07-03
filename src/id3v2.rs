use std::fmt::Display;

use crate::audio_info::{self, Tagged};

/// An ID3v2 tag.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    /// The tag's header, which includes size information.
    header: Header,

    /// The tag's frames, which contains the actual tag information.
    frames: Vec<Frame>,
}

impl Tag {
    /// Serialize this [`id3v2::Tag`] into bytes as a [`Vec<u8>`].
    ///
    /// [`id3v2::Tag`]: Tag
    /// [`Vec<u8>`]: Vec<u8>
    pub fn to_bytes(self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.header.to_bytes());
        bytes.append(
            &mut self
                .frames
                .into_iter()
                .flat_map(|frame| frame.to_bytes())
                .collect(),
        );

        bytes
    }
}

/// Parses the given [`u8`] [`slice`] into a [`Tag`]. This operation gives [`None`] if an only if
/// the header failes to parse.
///
/// [`u8`]: u8
/// [`slice`]: std::slice
/// [`Tag`]: Tag
/// [`None`]: Option::None
pub fn parse_tag(bytes: &[u8]) -> Result<(Tag, &[u8]), ParseError> {
    let (header, remaining_bytes) = Header::from_bytes(bytes)?;

    if remaining_bytes.len() < header.size as usize {
        return Err(ParseError::NotEnoughBytes);
    }

    let frames: Vec<Frame> = FrameIter {
        remaining_bytes: &remaining_bytes[..header.size as usize],
    }
    .collect();

    let remaining_bytes = &remaining_bytes[header.size as usize..];
    Ok((Tag { header, frames }, remaining_bytes))
}

impl Tagged for Tag {
    fn album_title(&self) -> Option<String> {
        find_frame(self, ID_ALBUM_TITLE).and_then(|f| f.text())
    }

    fn bpm(&self) -> Option<f64> {
        find_frame(self, ID_BPM)
            .and_then(|f| f.text())
            .and_then(|s| s.parse::<f64>().ok())
    }

    fn composer(&self) -> Option<String> {
        find_frame(self, ID_COMPOSER).and_then(|f| f.text())
    }

    fn content_type(&self) -> Option<String> {
        find_frame(self, ID_CONTENT_TYPE).and_then(|f| f.text())
    }

    fn copyright_message(&self) -> Option<String> {
        find_frame(self, ID_COPYRIGHT_MESSAGE).and_then(|f| f.text())
    }

    fn date(&self) -> Option<String> {
        find_frame(self, ID_DATE).and_then(|f| f.text())
    }

    fn playlist_delay(&self) -> Option<String> {
        find_frame(self, ID_PLAYLIST_DELAY).and_then(|f| f.text())
    }

    fn encoded_by(&self) -> Option<String> {
        find_frame(self, ID_ENCODED_BY).and_then(|f| f.text())
    }

    fn lyricist(&self) -> Option<String> {
        find_frame(self, ID_LYRICIST).and_then(|f| f.text())
    }

    fn file_type(&self) -> Option<String> {
        find_frame(self, ID_FILE_TYPE).and_then(|f| f.text())
    }

    fn time(&self) -> Option<String> {
        find_frame(self, ID_TIME).and_then(|f| f.text())
    }

    fn content_group_description(&self) -> Option<String> {
        find_frame(self, ID_CONTENT_GROUP_DESCRIPTION).and_then(|f| f.text())
    }

    fn title(&self) -> Option<String> {
        find_frame(self, ID_TITLE).and_then(|f| f.text())
    }

    fn subtitle(&self) -> Option<String> {
        find_frame(self, ID_SUBTITLE).and_then(|f| f.text())
    }

    fn initial_key(&self) -> Option<audio_info::Key> {
        find_frame(self, ID_INITIAL_KEY)
            .and_then(|f| f.text())
            .and_then(|t| audio_info::Key::parse_key(&t))
    }

    fn language(&self) -> Option<String> {
        find_frame(self, ID_LANGUAGE).and_then(|f| f.text())
    }

    fn length(&self) -> Option<String> {
        find_frame(self, ID_LENGTH).and_then(|f| f.text())
    }

    fn media_type(&self) -> Option<String> {
        find_frame(self, ID_MEDIA_TYPE).and_then(|f| f.text())
    }

    fn original_album(&self) -> Option<String> {
        find_frame(self, ID_ORIGINAL_ALBUM).and_then(|f| f.text())
    }

    fn original_filename(&self) -> Option<String> {
        find_frame(self, ID_ORIGINAL_FILENAME).and_then(|f| f.text())
    }

    fn original_artist(&self) -> Option<String> {
        find_frame(self, ID_ORIGINAL_ARTIST).and_then(|f| f.text())
    }

    fn original_release_year(&self) -> Option<u32> {
        find_frame(self, ID_ORIGINAL_RELEASE_YEAR)
            .and_then(|f| f.text())
            .and_then(|s| s.parse::<u32>().ok())
    }

    fn file_owner(&self) -> Option<String> {
        find_frame(self, ID_FILE_OWNER).and_then(|f| f.text())
    }

    fn lead_artist(&self) -> Option<String> {
        find_frame(self, ID_LEAD_ARTIST).and_then(|f| f.text())
    }

    fn band(&self) -> Option<String> {
        find_frame(self, ID_BAND).and_then(|f| f.text())
    }

    fn conductor(&self) -> Option<String> {
        find_frame(self, ID_CONDUCTOR).and_then(|f| f.text())
    }

    fn modified_by(&self) -> Option<String> {
        find_frame(self, ID_MODIFIED_BY).and_then(|f| f.text())
    }

    fn part_of_set(&self) -> Option<String> {
        find_frame(self, ID_PART_OF_SET).and_then(|f| f.text())
    }

    fn publisher(&self) -> Option<String> {
        find_frame(self, ID_PUBLISHER).and_then(|f| f.text())
    }

    fn track_number(&self) -> Option<u32> {
        find_frame(self, ID_TRACK_NUMBER)
            .and_then(|f| f.text())
            .and_then(|s| s.parse::<u32>().ok())
    }

    fn recording_date(&self) -> Option<String> {
        find_frame(self, ID_RECORDING_DATE).and_then(|f| f.text())
    }

    fn internet_radio_station(&self) -> Option<String> {
        find_frame(self, ID_INTERNET_RADIO_STATION_NAME).and_then(|f| f.text())
    }

    fn size(&self) -> Option<String> {
        find_frame(self, ID_SIZE).and_then(|f| f.text())
    }

    fn isrc(&self) -> Option<String> {
        find_frame(self, ID_ISRC).and_then(|f| f.text())
    }

    fn encoding_settings(&self) -> Option<String> {
        find_frame(self, ID_ENCODING_SETTINGS).and_then(|f| f.text())
    }

    fn year(&self) -> Option<u32> {
        find_frame(self, ID_YEAR)
            .and_then(|f| f.text())
            .and_then(|s| s.parse::<u32>().ok())
    }
}

/// Errors that may occur when parsing an ID3v2 tag.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum ParseError {
    /// Could not parse the header, which is required to continue parsing.
    CouldNotParseHeader,

    /// Parsed the header, which indicated that a header extension succeeds it, but one did not.
    NoHeaderExtension,

    /// Not enough bytes were provided to match or exceed the parsed header's size description of
    /// the ID3v2 tag.
    NotEnoughBytes,

    /// The expected ID3 tag header ID was not present, indicating that no ID3 tag exists.
    BadId,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::CouldNotParseHeader => {
                write!(f, "expected an ID3v2 header but could not parse it")
            }
            ParseError::NoHeaderExtension => write!(
                f,
                "expected an extension to the ID3v2 header but could not parse it"
            ),
            ParseError::NotEnoughBytes => write!(f, "not enough bytes to parse ID3v2 tag"),
            ParseError::BadId => write!(f, "expected ID3 tag ID was not present"),
        }
    }
}

/// Finds a [`Frame`] with the given [`FrameId`] within an ID3v2 [`Tag`].
///
/// [`Frame`]: Frame
/// [`FrameId`]: FrameId
/// [`Tag`]: Tag
fn find_frame(tag: &Tag, id: FrameId) -> Option<&Frame> {
    tag.frames.iter().find(|f| f.header.id == id)
}

/// Type alias for frame IDs, which are four bytes meant to be interpreted as text.
type FrameId = [u8; 4];

const ID_ALBUM_TITLE: FrameId = *b"TALB";
const ID_BPM: FrameId = *b"TBPM";
const ID_COMPOSER: FrameId = *b"TCOM";
const ID_CONTENT_TYPE: FrameId = *b"TCON";
const ID_COPYRIGHT_MESSAGE: FrameId = *b"TCOP";
const ID_DATE: FrameId = *b"TDAT";
const ID_PLAYLIST_DELAY: FrameId = *b"TDLY";
const ID_ENCODED_BY: FrameId = *b"TENC";
const ID_LYRICIST: FrameId = *b"TEXT";
const ID_FILE_TYPE: FrameId = *b"TFLT";
const ID_TIME: FrameId = *b"TIME";
const ID_CONTENT_GROUP_DESCRIPTION: FrameId = *b"TIT1";
const ID_TITLE: FrameId = *b"TIT2";
const ID_SUBTITLE: FrameId = *b"TIT3";
const ID_INITIAL_KEY: FrameId = *b"TKEY";
const ID_LANGUAGE: FrameId = *b"TLAN";
const ID_LENGTH: FrameId = *b"TLEN";
const ID_MEDIA_TYPE: FrameId = *b"TMED";
const ID_ORIGINAL_ALBUM: FrameId = *b"TOAL";
const ID_ORIGINAL_FILENAME: FrameId = *b"TOLY";
const ID_ORIGINAL_ARTIST: FrameId = *b"TOPE";
const ID_ORIGINAL_RELEASE_YEAR: FrameId = *b"TORY";
const ID_FILE_OWNER: FrameId = *b"TOWN";
const ID_LEAD_ARTIST: FrameId = *b"TPE1";
const ID_BAND: FrameId = *b"TPE2";
const ID_CONDUCTOR: FrameId = *b"TPE3";
const ID_MODIFIED_BY: FrameId = *b"TPE4";
const ID_PART_OF_SET: FrameId = *b"TPOS";
const ID_PUBLISHER: FrameId = *b"TPUB";
const ID_TRACK_NUMBER: FrameId = *b"TRCK";
const ID_RECORDING_DATE: FrameId = *b"TRDA";
const ID_INTERNET_RADIO_STATION_NAME: FrameId = *b"TRSN";
const ID_SIZE: FrameId = *b"TSIZ";
const ID_ISRC: FrameId = *b"TSRC";
const ID_ENCODING_SETTINGS: FrameId = *b"TSSE";
const ID_YEAR: FrameId = *b"TYER";

/// An [`Iterator`] over ID3v2 [`Frame`]s.
///
/// [`Iterator`]: Iterator
/// [`Frame`]: Frame
#[derive(Debug, Clone, Copy)]
struct FrameIter<'d> {
    remaining_bytes: &'d [u8],
}

impl<'d> Iterator for FrameIter<'d> {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        let (frame, remaining_bytes) = Frame::from_bytes(self.remaining_bytes)?;
        self.remaining_bytes = remaining_bytes;
        return Some(frame);
    }
}

/// ID3v2 header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Header {
    version: u16,
    flags: HeaderFlags,

    /// The size of the ID3v2 data, excluding this header, but including the extended header. `size`
    /// is really a 28 bit integer, as the most significant bit of each byte is ignored while
    /// reading.
    size: u32,

    /// Optional extended header.
    extended_header: Option<ExtendedHeader>,
}

/// Flags from an ID3v2 header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct HeaderFlags(u8);

/// Optional extension to the required [`id3v2::Header`].
///
/// [`id3v2::Header`]: Header
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExtendedHeader {
    /// Total size of the extended header, this is either 6 or 10 bytes, depending on whether CRC
    /// data is present. This value excludes itself, meaning that the actual size of the extended
    /// header is this value plus 4.
    extended_header_size: u32,

    extended_flags: ExtendedHeaderFlags,
    size_of_padding: u32,

    /// Optional CRC data.
    total_frame_crc: Option<u32>,
}

/// Flags from an ID3v2 extended header.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ExtendedHeaderFlags(u16);

/// A frame with a [`FrameHeader`], and unparsed data thereafter. The included `data` field will
/// have a length equal to the [`FrameHeader`]'s `size` field.
///
/// [`FrameHeader`]: FrameHeader
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Frame {
    /// The [`Frame`]'s header.
    ///
    /// [`Frame`]: Frame
    header: FrameHeader,

    /// The data following the [`FrameHeader`] which is included within the `size` field of the
    /// [`FrameHeader`].
    ///
    /// [`FrameHeader`]: FrameHeader
    data: Vec<u8>,
}

/// A header for ID3v2 frames.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FrameHeader {
    /// Frame IDs are made up of the capital letters 'A' through 'Z' as well as the digits '0'
    /// through '9'.
    id: [u8; 4],

    /// Size of the frame, not including this [`FrameHeader`].
    ///
    /// [`FrameHeader`]: FrameHeader
    size: u32,

    flags: FrameHeaderFlags,
}

/// Flags for ID3v2 frame headers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FrameHeaderFlags(u16);

impl Header {
    /// Parse a [`Header`] from the given bytes. Returns a [`Header`] and the remaining, unparsed
    /// bytes which come after.
    ///
    /// [`Header`]: Header
    fn from_bytes(bytes: &[u8]) -> Result<(Header, &[u8]), ParseError> {
        if bytes.len() < 10 {
            return Err(ParseError::NotEnoughBytes);
        }

        let identifier = &bytes[0..3];

        match identifier {
            b"ID3" => {
                let version = u16::from_be_bytes(
                    *bytes[3..5]
                        .as_array()
                        .ok_or(ParseError::CouldNotParseHeader)?,
                );

                let flags = HeaderFlags(*bytes.get(5).ok_or(ParseError::CouldNotParseHeader)?);

                // ID3v2 ignores the most significant bit of each byte in `size`, meaning that
                // `size` is really a 28 bit integer not a 32 bit integer.
                let size = u32::from_be_bytes(
                    *bytes[6..10]
                        .as_array()
                        .ok_or(ParseError::CouldNotParseHeader)?,
                );

                // extracts each of `size`'s four bytes.
                let size_0 = size & 0x_00_00_00_FF;
                let size_1 = size & 0x_00_00_FF_00;
                let size_2 = size & 0x_00_FF_00_00;
                let size_3 = size & 0x_FF_00_00_00;

                // recombine while downshifting each byte into the previous' most significant bit.
                let size = (size_3 >> 3) | (size_2 >> 2) | (size_1 >> 1) | size_0;

                let remaining_bytes = &bytes[10..];

                if flags.extended_header() {
                    let (extended_header, remaining_bytes) =
                        ExtendedHeader::from_bytes(remaining_bytes)?;

                    let header = Header {
                        version,
                        flags,
                        size,
                        extended_header: Some(extended_header),
                    };

                    Ok((header, remaining_bytes))
                } else {
                    let header = Header {
                        version,
                        flags,
                        size,
                        extended_header: None,
                    };

                    Ok((header, remaining_bytes))
                }
            }

            _ => Err(ParseError::BadId),
        }
    }

    /// Serialize this [`Header`] into bytes.
    ///
    /// [`Header`]: Header
    fn to_bytes(self) -> Vec<u8> {
        let mut bytes: Vec<u8> = vec![b'I', b'D', b'3'];
        bytes.append(&mut self.version.to_be_bytes().into());
        bytes.append(&mut self.flags.0.to_be_bytes().into());
        bytes.append(&mut self.size.to_be_bytes().into());

        if let Some(extended_header) = self.extended_header {
            bytes.append(&mut extended_header.to_bytes());
        }

        bytes
    }
}

impl ExtendedHeader {
    /// Parse an [`ExtendedHeader`] from the given slice of bytes. Returns the parsed
    /// [`ExtendedHeader`] and all remaining unparsed bytes after it.
    ///
    /// [`ExtendedHeader`]: ExtendedHeader
    fn from_bytes(bytes: &[u8]) -> Result<(ExtendedHeader, &[u8]), ParseError> {
        if bytes.len() < 10 {
            return Err(ParseError::NotEnoughBytes);
        }

        let extended_header_size = u32::from_be_bytes(
            *bytes[0..4]
                .as_array()
                .ok_or(ParseError::NoHeaderExtension)?,
        );

        let extended_flags = ExtendedHeaderFlags(u16::from_be_bytes(
            *bytes[4..6]
                .as_array()
                .ok_or(ParseError::NoHeaderExtension)?,
        ));

        let size_of_padding = u32::from_be_bytes(
            *bytes[6..10]
                .as_array()
                .ok_or(ParseError::NoHeaderExtension)?,
        );

        if extended_flags.crc_data_present() {
            if bytes.len() < 14 {
                return Err(ParseError::NotEnoughBytes);
            }

            let crc = u32::from_be_bytes(
                *bytes[10..14]
                    .as_array()
                    .ok_or(ParseError::NoHeaderExtension)?,
            );

            let extended_header = ExtendedHeader {
                extended_header_size,
                extended_flags,
                size_of_padding,
                total_frame_crc: Some(crc),
            };

            Ok((extended_header, &bytes[14..]))
        } else {
            let extended_header = ExtendedHeader {
                extended_header_size,
                extended_flags,
                size_of_padding,
                total_frame_crc: None,
            };

            Ok((extended_header, &bytes[10..]))
        }
    }

    /// Serialize the [`ExtendedHeader`] into bytes in a [`Vec<u8>`].
    ///
    /// [`ExtendedHeader`]: ExtendedHeader
    /// [`Vec<u8>`]: Vec<u8>
    fn to_bytes(self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.extended_header_size.to_be_bytes().into());
        bytes.append(&mut self.extended_flags.0.to_be_bytes().into());
        bytes.append(&mut self.size_of_padding.to_be_bytes().into());

        if let Some(total_frame_crc) = self.total_frame_crc {
            bytes.append(&mut total_frame_crc.to_be_bytes().into());
        }

        bytes
    }
}

impl Frame {
    /// Parse a [`Frame`] from the given bytes. Returns the parsed [`Frame`] and all unparsed bytes
    /// which follow it.
    ///
    /// [`Frame`]: Frame
    fn from_bytes(bytes: &[u8]) -> Option<(Frame, &[u8])> {
        let (header, bytes) = FrameHeader::from_bytes(bytes)?;

        // ID3v2 requires that a frame be at least one byte, excluding its header, therefor `size`
        // must be at least 1.
        if header.size as usize > bytes.len() || header.size < 1 || header.id == [0, 0, 0, 0] {
            return None;
        }

        let data = bytes[0..header.size as usize].to_vec();
        Some((Frame { header, data }, &bytes[header.size as usize..]))
    }

    /// Serialize the [`Frame`] into bytes as a [`Vec<u8>`].
    ///
    /// [`Frame`]: Frame
    /// [`Vec<u8>`]: Vec<u8>
    fn to_bytes(mut self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.header.to_bytes());
        bytes.append(&mut self.data);
        bytes
    }

    /// The size of the [`Frame`] in bytes after serialization.
    ///
    /// [`Frame`]: Frame
    fn serialized_size(&self) -> u32 {
        10 + self.data.len() as u32
    }

    /// Compute the text of a [`Frame`].
    ///
    /// [`Frame`]: Frame
    fn text(&self) -> Option<String> {
        let encoding_part = self.data[0];
        let text_part = &self.data[1..];

        match encoding_part {
            // ISO-8859-1
            0x00 => {
                let end = text_part
                    .iter()
                    .position(|&c| c == 0)
                    .unwrap_or(text_part.len());
                String::from_utf8(text_part[..end].to_vec()).ok()
            }

            // 16-bit unicode.
            0x01 => {
                let byte_order = &text_part[0..2];
                let text = &text_part[2..];
                let words: Vec<u16> = match byte_order {
                    [0xFF, 0xFE] => (0..text.len() / 2)
                        .map(|i| u16::from_le_bytes([text[i * 2], text[i * 2 + 1]]))
                        .collect(),
                    [0xFE, 0xFF] => (0..text.len() / 2)
                        .map(|i| u16::from_be_bytes([text[i * 2], text[i * 2 + 1]]))
                        .collect(),
                    _ => return None,
                };

                let end = words.iter().position(|&c| c == 0).unwrap_or(words.len());

                String::from_utf16(&words[..end]).ok()
            }

            _ => return None,
        }
    }
}

impl FrameHeader {
    /// Parse a [`FrameHeader`] from the given bytes. Returning the [`FrameHeader`] and all unparsed
    /// bytes which follow it.
    ///
    /// [`FrameHeader`]: FrameHeader
    fn from_bytes(bytes: &[u8]) -> Option<(FrameHeader, &[u8])> {
        if bytes.len() < 10 {
            return None;
        }

        let size = u32::from_be_bytes(*bytes[4..8].as_array()?);

        let size_0 = size & 0x_00_00_00_FF;
        let size_1 = size & 0x_00_00_FF_00;
        let size_2 = size & 0x_00_FF_00_00;
        let size_3 = size & 0x_FF_00_00_00;

        let size = (size_3 >> 3) | (size_2 >> 2) | (size_1 >> 1) | size_0;

        Some((
            FrameHeader {
                id: *bytes[0..4].as_array()?,
                size,
                flags: FrameHeaderFlags(u16::from_be_bytes(*bytes[8..10].as_array()?)),
            },
            &bytes[10..],
        ))
    }

    /// Serialize the [`FrameHeader`] into bytes as a [`Vec<u8>`].
    ///
    /// [`FrameHeader`]: FrameHeader
    /// [`Vec<u8>`]: Vec<u8>
    fn to_bytes(self) -> Vec<u8> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.append(&mut self.id.into());
        bytes.append(&mut self.size.to_be_bytes().into());
        bytes.append(&mut self.flags.0.to_be_bytes().into());
        bytes
    }
}

impl HeaderFlags {
    fn unsynchonisation(self) -> bool {
        self.0 & 0b_1000_000 > 0
    }

    fn extended_header(self) -> bool {
        self.0 & 0b_0100_0000 > 0
    }

    fn experimental_indicator(self) -> bool {
        self.0 & 0b_0010_0000 > 0
    }
}

impl ExtendedHeaderFlags {
    fn crc_data_present(self) -> bool {
        self.0 & 0b_1000_0000_0000_0000 > 0
    }
}

impl FrameHeaderFlags {
    fn discard_with_tag_alter(self) -> bool {
        self.0 & 0b_1000_0000_0000_0000 > 0
    }

    fn discard_with_file_alter(self) -> bool {
        self.0 & 0b_0100_0000_0000_0000 > 0
    }

    fn read_only(self) -> bool {
        self.0 & 0b_0010_0000_0000_0000 > 0
    }

    fn compressed(self) -> bool {
        self.0 & 0b_0000_0000_1000_0000 > 0
    }

    fn encrypted(self) -> bool {
        self.0 & 0b_0000_0000_0100_0000 > 0
    }

    fn contains_group_information(self) -> bool {
        self.0 & 0b_0000_0000_0010_0000 > 0
    }
}

#[cfg(test)]
mod tests {
    use crate::id3v2::ExtendedHeader;
    use crate::id3v2::ExtendedHeaderFlags;

    use super::Header;
    use super::HeaderFlags;

    #[test]
    #[rustfmt::skip]
    fn header_to_bytes() {
        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0000_0000),
            size: 0b_00001000_00100000_00000001_00000010,
            extended_header: None,
        };

        let bytes = header.to_bytes();

        assert_eq!(
            bytes,
            vec![
                // ID
                b'I', b'D', b'3', 

                // version
                0, 2,

                // flags
                0,

                // size
                0b00001000, 0b00100000, 0b00000001, 0b00000010
            ]
        );

        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0100_0000),
            size: 0b_00001000_00100000_00000001_00000010,
            extended_header: Some(ExtendedHeader {
                extended_header_size: 6,
                extended_flags: ExtendedHeaderFlags(0b_01010000_11110000),
                size_of_padding: 0,
                total_frame_crc: None,
            }),
        };

        let bytes = header.to_bytes();

        assert_eq!(
            bytes,
            vec![
                // ID
                b'I', b'D', b'3', 

                // version
                0, 2,

                // flags
                0b_0100_0000,

                // size
                0b00001000,0b00100000, 0b00000001, 0b00000010,

                // extended header size
                0, 0, 0, 6,

                // extended header flags
                0b01010000, 0b11110000,

                // size of padding
                0, 0, 0, 0
            ]
        );

        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0100_0000),
            size: 0b_00001000_00100000_00000001_00000010,
            extended_header: Some(ExtendedHeader {
                extended_header_size: 10,
                extended_flags: ExtendedHeaderFlags(0b_01010000_11110000),
                size_of_padding: 0,
                total_frame_crc: Some(0b_00001111_00110011_01010101_11110000),
            }),
        };

        let bytes = header.to_bytes();

        assert_eq!(
            bytes,
            vec![
                // ID
                b'I', b'D', b'3', 

                // version
                0, 2,

                // flags
                0b_0100_0000,

                // size
                0b00001000,0b00100000, 0b00000001, 0b00000010,

                // extended header size
                0, 0, 0, 10,

                // extended header flags
                0b01010000, 0b11110000,

                // size of padding
                0, 0, 0, 0,

                // crc
                0b00001111, 0b00110011, 0b01010101, 0b11110000,
            ]
        );
    }

    #[test]
    #[rustfmt::skip]
    fn header_from_bytes() {
        let bytes = [
            // ID
            b'I', b'D', b'3', 

            // version
            0, 2,

            // flags
            0,

            // size
            0, 0, 0, 0
        ];

        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0000_0000),
            size: 0,
            extended_header: None,
        };


        assert_eq!(Header::from_bytes(&bytes).map(|tuple| tuple.0), Ok(header));

        let bytes = [
            // ID
            b'I', b'D', b'3', 

            // version
            0, 2,

            // flags
            0b_0100_0000,

            // size
            0b00001000, 0b00100000, 0b00000001, 0b00000010,

            // extended header size
            0, 0, 0, 6,

            // extended header flags
            0b01010000, 0b11110000,

            // size of padding
            0, 0, 0, 0
        ];

        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0100_0000),
            size: 0b_00000001_00001000_00000000_10000010,
            extended_header: Some(ExtendedHeader {
                extended_header_size: 6,
                extended_flags: ExtendedHeaderFlags(0b_01010000_11110000),
                size_of_padding: 0,
                total_frame_crc: None,
            }),
        };


        assert_eq!(Header::from_bytes(&bytes).map(|tuple| tuple.0), Ok(header));

        let bytes = [
            // ID
            b'I', b'D', b'3', 

            // version
            0, 2,

            // flags
            0b_0100_0000,

            // size
            0, 0, 0, 14,

            // extended header size
            0, 0, 0, 10,

            // extended header flags
            0b10000000, 0b00000000,

            // size of padding
            0, 0, 0, 0,

            // crc
            0b00001111, 0b00110011, 0b01010101, 0b11110000,
        ];

        let header = Header {
            version: 2,
            flags: HeaderFlags(0b_0100_0000),
            size: 14,
            extended_header: Some(ExtendedHeader {
                extended_header_size: 10,
                extended_flags: ExtendedHeaderFlags(0b_10000000_00000000),
                size_of_padding: 0,
                total_frame_crc: Some(0b_00001111_00110011_01010101_11110000),
            }),
        };


        assert_eq!(Header::from_bytes(&bytes).map(|tuple| tuple.0), Ok(header));
    }
}
