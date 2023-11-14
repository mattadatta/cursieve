// Sieve

pub trait Sieve: SieveSift + SieveDisperse {
}

pub trait SieveSift {
    fn sift_cursor_at(cursor: &mut std::io::Cursor<&[u8]>, offset: u64) -> Result<Self, Error> where Self: Sized;
    fn sift_cursor(cursor: &mut std::io::Cursor<&[u8]>) -> Result<Self, Error> where Self: Sized {
        Self::sift_cursor_at(cursor, 0)
    }
    fn sift_at(data: &[u8], offset: u64) -> Result<Self, Error> where Self: Sized {
        let mut cursor = std::io::Cursor::new(data);
        Self::sift_cursor_at(&mut cursor, offset)
    }
    fn sift(data: &[u8]) -> Result<Self, Error> where Self: Sized {
        let mut cursor = std::io::Cursor::new(data);
        Self::sift_cursor(&mut cursor)
    }
}

pub trait SieveDisperse {
    fn disperse_cursor_at(&self, cursor: &mut std::io::Cursor<&mut [u8]>, offset: u64) -> Result<(), Error>;
    fn disperse_cursor(&self, cursor: &mut std::io::Cursor<&mut [u8]>) -> Result<(), Error> {
        self.disperse_cursor_at(cursor, 0)
    }
    fn disperse_at(&self, data: &mut [u8], offset: u64) -> Result<(), Error> {
        let mut cursor = std::io::Cursor::new(data);
        self.disperse_cursor_at(&mut cursor, offset)
    }
    fn disperse(&self, data: &mut [u8]) -> Result<(), Error> {
        let mut cursor = std::io::Cursor::new(data);
        self.disperse_cursor(&mut cursor)
    }
}

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
