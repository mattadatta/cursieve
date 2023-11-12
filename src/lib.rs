use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IoError")]
    IoError(#[from] std::io::Error),
    #[error("A Sieve error occurred: `{0}`")]
    Generic(String),
}

impl From<String> for Error {
    fn from(msg: String) -> Self {
        Error::Generic(msg)
    }
}

pub trait Sieve {
    fn sift(data: &[u8], offset: u64) -> Result<Self, Error> where Self: Sized;
}
