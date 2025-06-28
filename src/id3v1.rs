use crate::audio_info::{AudioTag, AudioTagged};
use std::mem::{self, transmute};

/// Packed struct for directly transmuting from bytes.
#[repr(packed)]
#[derive(PartialEq, Eq, Debug)]
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

impl AudioTagged for Tag {
    fn get_tag(&self, audio_tag: AudioTag) -> Option<String> {
        match audio_tag {
            AudioTag::AlbumTitle => String::from_utf8(self.album.to_vec()).ok(),
            AudioTag::Title => String::from_utf8(self.title.to_vec()).ok(),
            AudioTag::LeadArtist => String::from_utf8(self.artist.to_vec()).ok(),
            AudioTag::Year => String::from_utf8(self.year.to_vec()).ok(),
            _ => None,
        }
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
