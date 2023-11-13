#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("IoError")]
    IoError(#[from] std::io::Error),
    #[error("IoError")]
    TryFromError(String),
    #[error("A Sieve error occurred: `{0}`")]
    Generic(String),
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error::Generic(msg)
    }
}

pub trait Sieve {
    fn sift_cursor(cursor: &mut std::io::Cursor<&[u8]>, offset: u64) -> Result<Self, Error> where Self: Sized;
    fn sift(data: &[u8], offset: u64) -> Result<Self, Error> where Self: Sized {
        let mut cursor = std::io::Cursor::new(data);
        Self::sift_cursor(&mut cursor, offset)
    }
}
