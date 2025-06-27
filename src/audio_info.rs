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
pub trait AudioTagged {
    /// Get the tag information of the requested [`AudioTag`].
    ///
    /// [`AudioTag`]: AudioTag
    fn get_tag(&self, audio_tag: AudioTag) -> Option<String>;
}

/// A tag which can be attributed to an audio file. Includes information like the artist, title,
/// album, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum AudioTag {
    /// Title of the track's album.
    AlbumTitle,

    /// Beats per minute of the track.
    Bpm,
    Composer,
    ContentType,
    CopyrightMessage,
    Date,
    PlaylistDelay,
    EncodedBy,
    Lyricist,
    FileType,
    Time,
    ContentGroupDescription,
    Title,
    Subtitle,
    InitialKey,
    Language,
    Length,
    MediaType,
    OriginalAlbum,
    OriginalFilename,
    OrginalArtist,
    OriginalReleaseYear,
    FileOwner,

    /// The lead artist on the track. This is usually just "Artist" in most software.
    LeadArtist,
    Band,
    Conductor,
    ModifiedBy,
    PartOfSet,
    Publisher,

    /// Track number within the album or other greater collection.
    TrackNumber,
    RecordingDate,
    InternetRadioStationName,
    Size,
    Isrc,
    EncodingSettings,
    Year,
}
