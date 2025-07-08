use std::{fmt::Display, io};

/// Implements [`ReadTag`] for a given type using a given function to get an
/// [`Option<impl ReadTag>`] when given `self.
///
/// [`ReadTag`]: ReadTag
/// [`Option<impl ReadTag>`]: Option<impl ReadTag>
#[rustfmt::skip]
#[macro_export]
macro_rules! impl_read_tag {
    ($t:ty, $tag:expr) => {
        impl $crate::audio_info::ReadTag for $t {
            fn album_title(&self) -> Option<String> { ($tag(self)).and_then(|t| t.album_title()) }
            fn bpm(&self) -> Option<f64> { ($tag(self)).and_then(|t| t.bpm()) }
            fn composer(&self) -> Option<String> { ($tag(self)).and_then(|t| t.composer()) }
            fn content_type(&self) -> Option<String> { ($tag(self)).and_then(|t| t.content_type()) }
            fn copyright_message(&self) -> Option<String> { ($tag(self)).and_then(|t| t.copyright_message()) }
            fn date(&self) -> Option<String> { ($tag(self)).and_then(|t| t.date()) }
            fn playlist_delay(&self) -> Option<String> { ($tag(self)).and_then(|t| t.playlist_delay()) }
            fn encoded_by(&self) -> Option<String> { ($tag(self)).and_then(|t| t.encoded_by()) }
            fn lyricist(&self) -> Option<String> { ($tag(self)).and_then(|t| t.lyricist()) }
            fn file_type(&self) -> Option<String> { ($tag(self)).and_then(|t| t.file_type()) }
            fn time(&self) -> Option<String> { ($tag(self)).and_then(|t| t.time()) }
            fn content_group_description(&self) -> Option<String> { ($tag(self)).and_then(|t| t.content_group_description()) }
            fn title(&self) -> Option<String> { ($tag(self)).and_then(|t| t.title()) }
            fn subtitle(&self) -> Option<String> { ($tag(self)).and_then(|t| t.subtitle()) }
            fn initial_key(&self) -> Option<crate::audio_info::Key> { ($tag(self)).and_then(|t| t.initial_key()) }
            fn language(&self) -> Option<String> { ($tag(self)).and_then(|t| t.language()) }
            fn length(&self) -> Option<String> { ($tag(self)).and_then(|t| t.length()) }
            fn media_type(&self) -> Option<String> { ($tag(self)).and_then(|t| t.media_type()) }
            fn original_album(&self) -> Option<String> { ($tag(self)).and_then(|t| t.original_album()) }
            fn original_filename(&self) -> Option<String> { ($tag(self)).and_then(|t| t.original_filename()) }
            fn original_artist(&self) -> Option<String> { ($tag(self)).and_then(|t| t.original_artist()) }
            fn original_release_year(&self) -> Option<u32> { ($tag(self)).and_then(|t| t.original_release_year()) }
            fn file_owner(&self) -> Option<String> { ($tag(self)).and_then(|t| t.file_owner()) }
            fn lead_artist(&self) -> Option<String> { ($tag(self)).and_then(|t| t.lead_artist()) }
            fn band(&self) -> Option<String> { ($tag(self)).and_then(|t| t.band()) }
            fn conductor(&self) -> Option<String> { ($tag(self)).and_then(|t| t.conductor()) }
            fn modified_by(&self) -> Option<String> { ($tag(self)).and_then(|t| t.modified_by()) }
            fn part_of_set(&self) -> Option<String> { ($tag(self)).and_then(|t| t.part_of_set()) }
            fn publisher(&self) -> Option<String> { ($tag(self)).and_then(|t| t.publisher()) }
            fn track_number(&self) -> Option<$crate::audio_info::TrackNumber> { $tag(self).and_then(|t| t.track_number()) }
            fn recording_date(&self) -> Option<String> { ($tag(self)).and_then(|t| t.recording_date()) }
            fn internet_radio_station(&self) -> Option<String> { ($tag(self)).and_then(|t| t.internet_radio_station()) }
            fn size(&self) -> Option<String> { ($tag(self)).and_then(|t| t.size()) }
            fn isrc(&self) -> Option<String> { ($tag(self)).and_then(|t| t.isrc()) }
            fn encoding_settings(&self) -> Option<String> { ($tag(self)).and_then(|t| t.encoding_settings()) }
            fn year(&self) -> Option<u32> { ($tag(self)).and_then(|t| t.year()) }
        }
    };
}

/// Implements [`WriteTag`] for a given type using a given function to get an
/// [`Option<impl WriteTag>`] when given `self`.
///
/// [`ReadTag`]: ReadTag
/// [`Option<impl ReadTag>`]: Option<impl ReadTag>
#[rustfmt::skip]
#[macro_export]
macro_rules! impl_write_tag {
    ($t:ty, $tag:expr) => {
        impl $crate::audio_info::WriteTag for $t {
            fn write_album_title(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_album_title(value)
            }

            fn write_bpm(
                &mut self
                , value: Option<f64>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_bpm(value)
            }

            fn write_composer(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_composer(value)
            }

            fn write_content_type(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_content_type(value)
            }

            fn write_copyright_message(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_copyright_message(value)
            }

            fn write_date(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_date(value)
            }

            fn write_playlist_delay(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_playlist_delay(value)
            }

            fn write_encoded_by(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_encoded_by(value)
            }

            fn write_lyricist(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_lyricist(value)
            }

            fn write_file_type(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_file_type(value)
            }

            fn write_time(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_time(value)
            }

            fn write_content_group_description(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_content_group_description(value)
            }

            fn write_title(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_title(value)
            }

            fn write_subtitle(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_subtitle(value)
            }

            fn write_initial_key(
                &mut self
                , value: Option<crate::audio_info::Key>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_initial_key(value)
            }

            fn write_language(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_language(value)
            }

            fn write_length(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_length(value)
            }

            fn write_media_type(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_media_type(value)
            }

            fn write_original_album(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_original_album(value)
            }

            fn write_original_filename(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_original_filename(value)
            }

            fn write_original_artist(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_original_artist(value)
            }

            fn write_original_release_year(
                &mut self
                , value: Option<u32>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_original_release_year(value)
            }

            fn write_file_owner(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_file_owner(value)
            }

            fn write_lead_artist(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_lead_artist(value)
            }

            fn write_band(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_band(value)
            }

            fn write_conductor(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_conductor(value)
            }

            fn write_modified_by(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_modified_by(value)
            }

            fn write_part_of_set(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_part_of_set(value)
            }

            fn write_publisher(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_publisher(value)
            }

            fn write_track_number(
                &mut self
                , value: Option<$crate::audio_info::TrackNumber>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = $tag(self).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_track_number(value)
            }

            fn write_recording_date(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_recording_date(value)
            }

            fn write_internet_radio_station(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_internet_radio_station(value)
            }

            fn write_size(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_size(value)
            }

            fn write_isrc(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_isrc(value)
            }

            fn write_encoding_settings(
                &mut self
                , value: Option<String>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_encoding_settings(value)
            }

            fn write_year(
                &mut self
                , value: Option<u32>
            ) -> std::result::Result<(), $crate::audio_info::WriteTagError> {
                let tag = ($tag(self)).ok_or($crate::audio_info::WriteTagError::NoTag)?;
                tag.write_year(value)
            }

        }

    };

}

pub(crate) use impl_read_tag;
pub(crate) use impl_write_tag;

/// A trait for types which contain audio, and can give information about the audio and how it was
/// recorded/sampled.
pub trait Audio {
    /// Sample rate of the sound in hertz.
    fn sample_rate(&self) -> f64;

    /// Size of each sample in bits.
    fn sample_size(&self) -> u16;

    /// Number of channels in the audio.
    fn channels(&self) -> u16;
}

/// A trait for types which either contain audio or are associated with some audio and can give
/// information about the audio including but not limited to the artist, album, title, etc.
pub trait ReadTag {
    fn album_title(&self) -> Option<String>;
    fn bpm(&self) -> Option<f64>;
    fn composer(&self) -> Option<String>;
    fn content_type(&self) -> Option<String>;
    fn copyright_message(&self) -> Option<String>;
    fn date(&self) -> Option<String>;
    fn playlist_delay(&self) -> Option<String>;
    fn encoded_by(&self) -> Option<String>;
    fn lyricist(&self) -> Option<String>;
    fn file_type(&self) -> Option<String>;
    fn time(&self) -> Option<String>;
    fn content_group_description(&self) -> Option<String>;
    fn title(&self) -> Option<String>;
    fn subtitle(&self) -> Option<String>;
    fn initial_key(&self) -> Option<Key>;
    fn language(&self) -> Option<String>;
    fn length(&self) -> Option<String>;
    fn media_type(&self) -> Option<String>;
    fn original_album(&self) -> Option<String>;
    fn original_filename(&self) -> Option<String>;
    fn original_artist(&self) -> Option<String>;
    fn original_release_year(&self) -> Option<u32>;
    fn file_owner(&self) -> Option<String>;
    fn lead_artist(&self) -> Option<String>;
    fn band(&self) -> Option<String>;
    fn conductor(&self) -> Option<String>;
    fn modified_by(&self) -> Option<String>;
    fn part_of_set(&self) -> Option<String>;
    fn publisher(&self) -> Option<String>;
    fn track_number(&self) -> Option<TrackNumber>;
    fn recording_date(&self) -> Option<String>;
    fn internet_radio_station(&self) -> Option<String>;
    fn size(&self) -> Option<String>;
    fn isrc(&self) -> Option<String>;
    fn encoding_settings(&self) -> Option<String>;
    fn year(&self) -> Option<u32>;
}

/// Trait for tags on audio or other types containing audio tag info which can be
/// written/overwritten.
pub trait WriteTag {
    fn write_album_title(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_bpm(&mut self, value: Option<f64>) -> Result<(), WriteTagError>;
    fn write_composer(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_content_type(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_copyright_message(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_date(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_playlist_delay(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_encoded_by(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_lyricist(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_file_type(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_time(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_content_group_description(
        &mut self,
        value: Option<String>,
    ) -> Result<(), WriteTagError>;
    fn write_title(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_subtitle(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_initial_key(&mut self, value: Option<Key>) -> Result<(), WriteTagError>;
    fn write_language(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_length(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_media_type(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_original_album(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_original_filename(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_original_artist(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_original_release_year(&mut self, value: Option<u32>) -> Result<(), WriteTagError>;
    fn write_file_owner(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_lead_artist(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_band(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_conductor(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_modified_by(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_part_of_set(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_publisher(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_track_number(&mut self, value: Option<TrackNumber>) -> Result<(), WriteTagError>;
    fn write_recording_date(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_internet_radio_station(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_size(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_isrc(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_encoding_settings(&mut self, value: Option<String>) -> Result<(), WriteTagError>;
    fn write_year(&mut self, value: Option<u32>) -> Result<(), WriteTagError>;
}

/// Errors that may occur while writing to audio tags with a [`WriteTag`] instance.
///
/// [`WriteTag`]: WriteTag
#[derive(Debug)]
pub enum WriteTagError {
    /// Tag was not present.
    NoTag,

    /// The field is not supported by the [`WriteTag`] instance.
    ///
    /// [`WriteTag`]: WriteTag
    FieldNotSupported,

    /// The given value is too long/large for the desired field on the [`WriteTag`] instance.
    ///
    /// [`WriteTag`]: WriteTag
    ValueTooLarge,

    /// The given value is not supported for the desired field on the [`WriteTag`] instance.
    ///
    /// [`WriteTag`]: WriteTag
    InvalidValue,

    /// An [`io::Error`] occured.
    ///
    /// [`io::Error`]: io::Error
    Io(io::Error),
}

/// A "track number" which supports including the number of items in the set containing the track.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TrackNumber {
    /// The position of the track.
    pub track: u32,

    /// The number of tracks/items in their containing set. This value is optional.
    pub of: Option<u32>,
}

impl Display for TrackNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.of {
            Some(of) => write!(f, "{}/{}", self.track, of),
            None => write!(f, "{}", self.track),
        }
    }
}

/// Represents a musical key. This type's implementation of [`Eq`] does not consider musically
/// equivilent keys to be equivilent keys, so [`Key::DFlat`] is not equal to [`Key::CSharp`]. To
/// compare keys for musical equivilance use [`Key::is_same_key`].
///
/// The [`Ord`] implementation of this type follows the circle of fifths. Starting at 'C' and going
/// clockwise adding sharps to the key. For the keys with expressions in both sharps and flats the
/// expression with sharps come first:
///
/// ```
/// use audio_tag::audio_info::Key;
/// assert!(Key::B < Key::CFlat);
/// ```
///
/// After the major keys the minor keys follow the same pattern.
///
/// [`Eq`]: Eq
/// [`Ord`]: Ord
/// [`Key::AFlat`]: Key::AFlat
/// [`Key::GSharp`]: Key::GSharp
/// [`Key::is_same_key`]: Key::is_same_key
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum Key {
    /// C.
    C,
    /// G.
    G,
    /// D.
    D,
    /// A.
    A,
    /// E.
    E,
    /// B.
    B,
    /// C Flat.
    CFlat,
    /// F Sharp.
    FSharp,
    /// G Flat.
    GFlat,
    /// C Sharp.
    CSharp,
    /// D Flat.
    DFlat,
    /// A Flat.
    AFlat,
    /// E Flat.
    EFlat,
    /// B Flat.
    BFlat,
    /// F.
    F,

    /// A Minor.
    AMinor,
    /// E Minor.
    EMinor,
    /// B Minor.
    BMinor,
    /// F Sharp Minor.
    FSharpMinor,
    /// C Sharp Minor.
    CSharpMinor,
    /// G Sharp Minor.
    GSharpMinor,
    /// E Flat Minor.
    EFlatMinor,
    /// B Flat Minor.
    BFlatMinor,
    /// F Minor.
    FMinor,
    /// C Minor.
    CMinor,
    /// G Minor.
    GMinor,
    /// D Minor.
    DMinor,

    /// Off key.
    OffKey,
}

impl Key {
    /// Produces the [`String`] which would be expected as input to [`Key::parse_key`].
    ///
    /// [`String`]: String
    /// [`Key::parse_key`]: Key::parse_key
    pub fn to_string(self) -> String {
        match self {
            Key::C => "C",
            Key::G => "G",
            Key::D => "D",
            Key::A => "A",
            Key::E => "E",
            Key::B => "B",
            Key::CFlat => "Cb",
            Key::FSharp => "F#",
            Key::GFlat => "Gb",
            Key::CSharp => "C#",
            Key::DFlat => "Db",
            Key::AFlat => "Ab",
            Key::EFlat => "Eb",
            Key::BFlat => "Bb",
            Key::F => "F",
            Key::AMinor => "Am",
            Key::EMinor => "Em",
            Key::BMinor => "Bm",
            Key::FSharpMinor => "F#m",
            Key::CSharpMinor => "C#m",
            Key::GSharpMinor => "G#m",
            Key::EFlatMinor => "Ebm",
            Key::BFlatMinor => "Bbm",
            Key::FMinor => "Fm",
            Key::CMinor => "Cm",
            Key::GMinor => "Gm",
            Key::DMinor => "Dm",
            Key::OffKey => "o",
        }
        .to_string()
    }

    /// Equates [`Key::B`] and [`Key::CFlat`], as well as other keys which can be equivilently
    /// expressed with either sharps or flats. This function _does not_ equat any minor keys to
    /// major keys.
    ///
    /// [`Key::B`]: Key::B
    /// [`Key::CFlat`]: Key::CFlat
    pub fn is_same_key(&self, other: &Key) -> bool {
        let min = self.min(other);
        let max = self.max(other);

        match (min, max) {
            (Key::B, Key::CFlat) => true,
            (Key::FSharp, Key::GFlat) => true,
            (Key::CSharp, Key::DFlat) => true,
            _ if max == min => true,
            _ => false,
        }
    }
    /// Parse a [`Key`] from the given text. This function follows ID3v2's spec for keys, where the
    /// ground key is represented as one of 'A', 'B', 'C', 'D', 'E', 'F', or 'G', and may optionaly
    /// be followed by 'b' to represent "flat", '#' to represent sharp, and/or 'm' to represent a
    /// minor key.
    pub fn parse_key(text: &str) -> Option<Key> {
        match text {
            "Ab" => Some(Key::AFlat),
            "A" => Some(Key::A),
            "Am" => Some(Key::AMinor),
            "Bb" => Some(Key::BFlat),
            "Bbm" => Some(Key::BFlatMinor),
            "B" => Some(Key::B),
            "Bm" => Some(Key::BMinor),
            "C" => Some(Key::C),
            "Cm" => Some(Key::CMinor),
            "C#" => Some(Key::CSharp),
            "C#m" => Some(Key::CSharpMinor),
            "Db" => Some(Key::DFlat),
            "D" => Some(Key::D),
            "Dm" => Some(Key::DMinor),
            "Eb" => Some(Key::EFlat),
            "E" => Some(Key::E),
            "Em" => Some(Key::EMinor),
            "F" => Some(Key::F),
            "Fm" => Some(Key::FMinor),
            "F#" => Some(Key::FSharp),
            "F#m" => Some(Key::FSharpMinor),
            "Gb" => Some(Key::GFlat),
            "G" => Some(Key::G),
            "Gm" => Some(Key::GMinor),
            "G#m" => Some(Key::GSharpMinor),
            "o" => Some(Key::OffKey),
            _ => None,
        }
    }
}
