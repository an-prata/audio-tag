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
pub trait Tagged {
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
    fn track_number(&self) -> Option<u32>;
    fn recording_date(&self) -> Option<String>;
    fn internet_radio_station(&self) -> Option<String>;
    fn size(&self) -> Option<String>;
    fn isrc(&self) -> Option<String>;
    fn encoding_settings(&self) -> Option<String>;
    fn year(&self) -> Option<u32>;
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
            "Cm#" => Some(Key::CSharpMinor),
            "Db" => Some(Key::DFlat),
            "D" => Some(Key::D),
            "Dm" => Some(Key::DMinor),
            "Eb" => Some(Key::EFlat),
            "E" => Some(Key::E),
            "Em" => Some(Key::EMinor),
            "F" => Some(Key::F),
            "Fm" => Some(Key::FMinor),
            "F#" => Some(Key::FSharp),
            "Fm#" => Some(Key::FSharpMinor),
            "Gb" => Some(Key::GFlat),
            "G" => Some(Key::G),
            "Gm" => Some(Key::GMinor),
            "Gm#" => Some(Key::GSharpMinor),
            "o" => Some(Key::OffKey),
            _ => None,
        }
    }
}
