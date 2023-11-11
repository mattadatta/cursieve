use thiserror::Error

#[derive(Debug, Clone, Error)]
pub enum Error {
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
