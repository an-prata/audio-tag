use crate::audio_info::{Key, ReadTag, TrackNumber, WriteTag, WriteTagError};
use std::{
    ascii,
    mem::{self, transmute},
};

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

impl ReadTag for Tag {
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

    fn initial_key(&self) -> Option<Key> {
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
        String::from_utf8(self.artist.to_vec()).ok()
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

    fn track_number(&self) -> Option<TrackNumber> {
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

impl WriteTag for Tag {
    fn write_album_title(&mut self, value: Option<String>) -> Result<(), WriteTagError> {
        let album = match value {
            Some(a) => a,
            None => return Err(WriteTagError::InvalidValue),
        };

        let bytes: Vec<u8> = album
            .chars()
            .filter_map(|c| c.as_ascii().map(|ac: ascii::Char| ac.to_u8()))
            .collect();

        if bytes.len() > self.album.len() {
            return Err(WriteTagError::ValueTooLarge);
        }

        self.album = [0; _];

        for i in 0..bytes.len() {
            self.album[i] = bytes[i];
        }

        Ok(())
    }

    fn write_bpm(&mut self, _value: Option<f64>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_composer(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_content_type(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_copyright_message(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_date(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_playlist_delay(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_encoded_by(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_lyricist(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_file_type(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_time(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_content_group_description(
        &mut self,
        _value: Option<String>,
    ) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_title(&mut self, value: Option<String>) -> Result<(), WriteTagError> {
        let title = match value {
            Some(t) => t,
            None => return Err(WriteTagError::InvalidValue),
        };

        let bytes: Vec<u8> = title
            .chars()
            .filter_map(|c| c.as_ascii().map(|ac: ascii::Char| ac.to_u8()))
            .collect();

        if bytes.len() > self.title.len() {
            return Err(WriteTagError::ValueTooLarge);
        }

        self.title = [0; _];

        for i in 0..bytes.len() {
            self.title[i] = bytes[i];
        }

        Ok(())
    }

    fn write_subtitle(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_initial_key(&mut self, _value: Option<Key>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_language(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_length(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_media_type(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_album(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_filename(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_artist(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_original_release_year(&mut self, _value: Option<u32>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_file_owner(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_lead_artist(&mut self, value: Option<String>) -> Result<(), WriteTagError> {
        let artist = match value {
            Some(a) => a,
            None => return Err(WriteTagError::InvalidValue),
        };

        let bytes: Vec<u8> = artist
            .chars()
            .filter_map(|c| c.as_ascii().map(|ac: ascii::Char| ac.to_u8()))
            .collect();

        if bytes.len() > self.artist.len() {
            return Err(WriteTagError::ValueTooLarge);
        }

        self.artist = [0; _];

        for i in 0..bytes.len() {
            self.artist[i] = bytes[i];
        }

        Ok(())
    }

    fn write_band(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_conductor(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_modified_by(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_part_of_set(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_publisher(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_track_number(&mut self, _value: Option<TrackNumber>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_recording_date(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_internet_radio_station(
        &mut self,
        _value: Option<String>,
    ) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_size(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_isrc(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_encoding_settings(&mut self, _value: Option<String>) -> Result<(), WriteTagError> {
        Err(WriteTagError::FieldNotSupported)
    }

    fn write_year(&mut self, value: Option<u32>) -> Result<(), WriteTagError> {
        let year = match value {
            Some(y) => y,
            None => return Err(WriteTagError::InvalidValue),
        };

        let text: String = year.to_string();

        if text.chars().any(|c| !c.is_ascii_digit()) {
            return Err(WriteTagError::InvalidValue);
        }

        let mut bytes: Vec<u8> = text
            .chars()
            .map(|c| c.as_ascii().map(|ac| ac.to_u8()))
            .try_collect()
            .ok_or(WriteTagError::InvalidValue)?;

        while bytes.len() < 4 {
            bytes.insert(0, b'0');
        }

        self.year = *bytes.as_array().ok_or(WriteTagError::ValueTooLarge)?;
        Ok(())
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
