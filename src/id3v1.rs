use crate::tags::{AudioTag, AudioTagged};
use std::mem;

/// Packed struct for directly transmuting from bytes.
#[repr(packed)]
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
    fn from_bytes(bytes: &[u8]) -> Option<(Tag, &[u8])> {
        if bytes.len() < size_of::<Tag>() {
            return None;
        }

        Some((
            Tag::from_bytes_exact(*bytes[..size_of::<Tag>()].as_array()?),
            &bytes[size_of::<Tag>()..],
        ))
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
    use crate::id3v1::Tag;

    #[test]
    fn tag_size() {
        // ID3v1 standard is exactly and always 128 bytes.
        assert_eq!(size_of::<Tag>(), 128);
    }
}
