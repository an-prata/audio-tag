use crate::audio_info::Tagged;
use std::mem::{self, transmute};

/// Packed struct for directly transmuting from bytes.
#[repr(packed)]
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct Tag {
    id: [u8; 3],
    title: [u8; 30],
    artist: [u8; 30],
    album: [u8; 30],
    year: [u8; 4],
    comment: [u8; 30],
    genre: u8,
}

/// Parse a [`Tag`] from the given bytes.
///
/// [`Tag`]: Tag
pub fn parse_tag(bytes: &[u8]) -> Option<(Tag, &[u8])> {
    Tag::from_bytes(bytes)
}

impl Tag {
    /// Creates a [`Tag`] from the given bytes, returning [`None`] if the [`slice`] is not long
    /// enough.
    ///
    /// [`Tag`]: Tag
    /// [`None`]: Option::None
    /// [`slice`]: std::slice
    pub fn from_bytes(bytes: &[u8]) -> Option<(Tag, &[u8])> {
        if bytes.len() < size_of::<Tag>() {
            return None;
        }

        Some((
            Tag::from_bytes_exact(*bytes[..size_of::<Tag>()].as_array()?),
            &bytes[size_of::<Tag>()..],
        ))
    }

    /// Serialize the [`id3v1::Tag`] into bytes.
    ///
    /// [`id3v1::Tag`]: Tag
    pub fn to_bytes(self) -> [u8; size_of::<Tag>()] {
        unsafe { transmute(self) }
    }

    /// Creates a [`Tag`] from an exact number of bytes.
    ///
    /// [`Tag`]: Tag
    fn from_bytes_exact(bytes: [u8; 128]) -> Tag {
        assert_eq!(size_of_val(&bytes), size_of::<Tag>());
        unsafe { mem::transmute(bytes) }
    }
}

impl Tagged for Tag {
    fn album_title(&self) -> Option<String> {
        String::from_utf8(self.album.to_vec()).ok()
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
        None
    }

    fn date(&self) -> Option<String> {
        None
    }

    fn playlist_delay(&self) -> Option<String> {
        None
    }

    fn encoded_by(&self) -> Option<String> {
        None
    }

    fn lyricist(&self) -> Option<String> {
        None
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
        String::from_utf8(self.title.to_vec()).ok()
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
        None
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
        None
    }

    fn publisher(&self) -> Option<String> {
        None
    }

    fn track_number(&self) -> Option<u32> {
        None
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
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tag_size() {
        // ID3v1 standard is exactly and always 128 bytes.
        assert_eq!(size_of::<Tag>(), 128);
    }

    #[test]
    fn to_bytes() {
        let tag = Tag {
            id: *b"TAG",
            title: [0b_1000_0000; _],
            artist: [0b_0100_0000; _],
            album: [0b_0010_0000; _],
            year: [0b_0001_0000; _],
            comment: [0b_0000_1000; _],
            genre: 0b_0000_0100,
        };

        let mut bytes = vec![b'T', b'A', b'G'];
        bytes.append(&mut [0b_1000_0000; 30].to_vec());
        bytes.append(&mut [0b_0100_0000; 30].to_vec());
        bytes.append(&mut [0b_0010_0000; 30].to_vec());
        bytes.append(&mut [0b_0001_0000; 4].to_vec());
        bytes.append(&mut [0b_0000_1000; 30].to_vec());
        bytes.push(0b_0000_0100);

        assert_eq!(bytes, tag.to_bytes());
    }

    #[test]
    fn from_bytes() {
        let mut bytes = vec![b'T', b'A', b'G'];
        bytes.append(&mut [0b_1000_0000; 30].to_vec());
        bytes.append(&mut [0b_0100_0000; 30].to_vec());
        bytes.append(&mut [0b_0010_0000; 30].to_vec());
        bytes.append(&mut [0b_0001_0000; 4].to_vec());
        bytes.append(&mut [0b_0000_1000; 30].to_vec());
        bytes.push(0b_0000_0100);

        let tag = Tag {
            id: *b"TAG",
            title: [0b_1000_0000; _],
            artist: [0b_0100_0000; _],
            album: [0b_0010_0000; _],
            year: [0b_0001_0000; _],
            comment: [0b_0000_1000; _],
            genre: 0b_0000_0100,
        };

        assert_eq!(Tag::from_bytes(&bytes).map(|tuple| tuple.0), Some(tag));
    }
}
