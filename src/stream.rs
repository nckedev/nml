#![allow(dead_code)]
use std::{collections::VecDeque, iter::Peekable};

use crate::{diagnostics::DiagEntry, parser::EndOfStream};

pub struct Stream<T, I>
where
    T: Clone + PartialEq + std::fmt::Debug,
    I: Iterator<Item = T>,
{
    buffer: VecDeque<T>,
    iter: Peekable<I>,
}

impl<T, I> Stream<T, I>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    I: Iterator<Item = T>,
{
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
            buffer: VecDeque::default(),
        }
    }
    /// Returns the next value without moving forward forward in the stream
    pub fn peek(&mut self) -> Option<&T> {
        match self.buffer.front() {
            None => self.iter.peek(),
            x => x,
        }
    }

    /// peeks n steps ahead and returns the value without moving forward in the stream
    pub fn peek_n(&mut self, steps: usize) -> Option<&T> {
        if self.buffer.len() > steps {
            self.buffer.get(steps)
        } else {
            let diff = steps - self.buffer.len();
            for _ in 0..=diff {
                match self.iter.next() {
                    Some(v) => self.buffer.push_back(v),
                    _ => break,
                };
            }
            self.buffer.get(steps)
        }
    }

    pub fn peek_expect(&mut self, pred: fn(&T) -> bool) -> bool {
        if let Some(v) = self.peek()
            && pred(v)
        {
            return true;
        }
        false
    }

    pub fn peek_n_expect(&mut self, steps: usize, pred: fn(&T) -> bool) -> bool {
        for _ in 0..=steps {
            match self.iter.next() {
                Some(v) => self.buffer.push_back(v),
                None => break,
            }
        }
        if let Some(v) = self.buffer.get(steps)
            && pred(v)
        {
            return true;
        }
        false
    }

    /// peeks at the next element and steps forward if it matches,
    /// returns true if a matching element was found
    pub fn peek_and_step_if(&mut self, pred: impl Into<T>) -> bool
    where
        T: PartialEq,
    {
        self.take_if(pred.into()).is_some()
    }

    pub fn peek_and_step_if_fn(&mut self, pred: fn(&T) -> bool) -> bool {
        self.take_if_fn(pred).is_some()
    }

    /// takes the elemnt at the front and returns it
    pub fn take(&mut self) -> Option<T> {
        if !self.buffer.is_empty() {
            self.buffer.pop_front()
        } else {
            self.iter.next()
        }
        // let v = self.buffer.pop_front();
        // if let Some(ref ch) = v {
        //     self.step_index(&Some(ch.clone()));
        // }
        // v
    }

    /// takes a element of the stream,
    /// returns Ok if there is an element
    /// Err if not
    pub fn take_or<E>(&mut self, err: E) -> Result<T, E> {
        if let Some(x) = self.take() {
            return Ok(x);
        };
        Err(err)
    }

    pub fn take_expecting_or_fn(
        &mut self,
        expecting: impl FnOnce(&T) -> bool,
        callback: impl FnOnce(&T),
    ) -> Option<T> {
        if let Some(t) = self.take() {
            if expecting(&t) {
                return Some(t);
            } else {
                callback(&t);
                return None;
            }
        }
        None
    }

    pub fn take_if(&mut self, pred: T) -> Option<T>
    where
        T: PartialEq,
    {
        if let Some(v) = self.peek()
            && *v == pred
        {
            return self.take();
        }
        None
    }

    pub fn take_if_fn(&mut self, pred: fn(&T) -> bool) -> Option<T> {
        if let Some(v) = self.peek()
            && pred(v)
        {
            return self.take();
        }
        None
    }

    /// Takes an item of the stream and returns it if it matches the expecations.
    /// Returns an error if the the stream is empty or the element did not meet the expectation.
    /// There is ALWAYS one element taken of the stream regardless of success or not.
    pub fn take_expecting<U, E>(&mut self, pred: impl Fn(T) -> Result<U, E>) -> Result<U, E>
    where
        E: TryInto<DiagEntry>,
        E: EndOfStream,
    {
        let Some(v) = self.take() else {
            return Err(E::end_of_stream());
        };

        pred(v)
    }

    pub fn take_if_expecting<U, E>(&mut self, pred: impl Fn(T) -> Result<U, E>) -> Result<U, E>
    where
        E: TryInto<DiagEntry>,
        E: EndOfStream,
    {
        match self.peek_expecting(&pred) {
            Ok(_) => pred(self.take().unwrap()),
            e => e,
        }
    }

    /// Peeks the top item of the stream and returns a copy it if it matches the expecations.
    /// Returns an error if the the stream is empty or the element did not meet the expectation.
    /// There is NEVER any element taken of the stream regardless of success or not.
    pub fn peek_expecting<U, E>(&mut self, pred: impl Fn(T) -> Result<U, E>) -> Result<U, E>
    where
        E: TryInto<DiagEntry>,
        E: EndOfStream,
    {
        let Some(v) = self.peek() else {
            return Err(E::end_of_stream());
        };
        pred(v.clone())
    }
    //
    // pub fn take_until_iter(&mut self, pred: fn(&T) -> bool) -> impl Iterator<Item = T> + '_ {
    //     StreamTakeIterator {
    //         stream: self,
    //         pred,
    //         invert_pred: true,
    //     }
    // }
    //
    // pub fn take_until(&mut self, pred: fn(&T) -> bool) -> Vec<T> {
    //     self.take_until_iter(pred).collect()
    // }

    /// takes item while the predicate is true
    pub fn take_while_iter(&mut self, pred: fn(&T) -> bool) -> impl Iterator<Item = T> + '_ {
        StreamTakeIterator {
            iter: &mut self.iter,
            pred,
            invert_pred: false,
        }
    }

    /// see [take_while_iter()]
    pub fn take_while(&mut self, pred: fn(&T) -> bool) -> Vec<T> {
        self.take_while_iter(pred).collect()
    }

    pub fn skip_until(&mut self, pred: fn(&T) -> bool) {
        while self.take_if_fn(pred).is_some() {}
    }
}

// impl<T> From<Vec<T>> for Stream<T>
// where
//     T: Clone,
//     T: PartialEq,
//     T: std::fmt::Debug,
//     T: Default,
//     T: LineSeparator<Item = T>,
// {
//     fn from(value: Vec<T>) -> Self {
//         Self {
//             buffer: VecDeque::from(value),
//         }
//     }
// }

struct StreamTakeIterator<'a, T, I>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    I: Iterator<Item = T>,
{
    iter: &'a mut Peekable<I>,
    pred: fn(&T) -> bool,
    invert_pred: bool,
}

impl<T, I> Iterator for StreamTakeIterator<'_, T, I>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.iter.peek() {
            if self.invert_pred {
                if !(self.pred)(x) {
                    return self.iter.next();
                }
            } else {
                if (self.pred)(x) {
                    return self.iter.next();
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, PartialEq, Debug, Default)]
    struct TestWrapper {
        value: i32,
    }

    fn generate_test_stream(len: i32) -> Stream<TestWrapper, impl Iterator<Item = TestWrapper>> {
        let v = (1..=len)
            .map(|x| TestWrapper { value: x })
            .collect::<Vec<TestWrapper>>();
        Stream::new(v.into_iter())
    }

    // #[test]
    // fn take_until() {
    //     let mut s = generate_test_stream(10);
    //     let t = s.take_until_iter(|x| x.value == 3).last();
    //     assert_eq!(Some(TestWrapper { value: 2 }), t);
    // }

    #[test]
    fn take_while() {
        let mut s = generate_test_stream(10);
        let t = s.take_while_iter(|x| x.value < 3).last();
        assert_eq!(Some(TestWrapper { value: 2 }), t);
    }

    #[test]
    fn peek_expect() {
        let mut s = generate_test_stream(10);
        assert!(s.peek_expect(|x| x.value == 1));
        assert!(!s.peek_expect(|x| x.value == 2));
    }

    #[test]
    fn peek() {
        let mut s = generate_test_stream(10);
        assert_eq!(Some(&TestWrapper { value: 1 }), s.peek());
    }

    #[test]
    fn peek_n() {
        let mut s = generate_test_stream(10);
        assert_eq!(Some(&TestWrapper { value: 2 }), s.peek_n(1));
    }
    #[test]
    fn peek_n_expect() {
        let mut s = generate_test_stream(10);
        assert!(s.peek_n_expect(1, |x| x.value == 2));
    }

    #[test]
    fn take() {
        let mut s = generate_test_stream(2);
        assert_eq!(Some(TestWrapper { value: 1 }), s.take());
        assert_eq!(Some(TestWrapper { value: 2 }), s.take());
        assert_eq!(None, s.take());
    }

    #[test]
    fn take_if() {
        let mut s = generate_test_stream(2);
        assert_eq!(None, s.take_if_fn(|x| x.value == 2));
        assert_eq!(Some(TestWrapper { value: 1 }), s.take());
        assert_eq!(
            Some(TestWrapper { value: 2 }),
            s.take_if_fn(|x| x.value == 2)
        );
    }
}
