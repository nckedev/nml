pub fn print(str: &str) -> () {
    println!("{}", str)
}

pub fn assert(predicate: bool, message: &str) {
    if !predicate {
        panic!("{}", message)
    }
}
pub fn test() -> i32 {
    2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assert() {
        assert_eq!(test(), 2);
    }
}
