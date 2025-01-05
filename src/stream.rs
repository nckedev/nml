#![allow(dead_code)]
use std::collections::VecDeque;

use crate::source_char::SourceIndex;
pub trait LineSeparator {
    type Item;
    fn is_line_separator(x: &Self::Item) -> bool;
}

pub struct Stream<T>
where
    T: Clone + PartialEq + std::fmt::Debug + LineSeparator<Item = T>,
{
    buffer: VecDeque<T>,
    pub index: SourceIndex,
}

impl<'a, T> Stream<T>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    T: LineSeparator<Item = T>,
{
    /// Returns the next value without moving forward forward in the stream
    pub fn peek(&self) -> Option<T> {
        if let Some(v) = self.buffer.get(0) {
            return Some(v.clone());
        }
        None
    }

    /// peeks n steps ahead and returns the value without moving forward in the stream
    pub fn peek_n(&self, steps: usize) -> Option<T> {
        if let Some(v) = self.buffer.get(steps) {
            return Some(v.clone());
        }
        None
    }

    pub fn peek_expect(&self, pred: fn(&T) -> bool) -> bool {
        if let Some(v) = self.buffer.get(0) {
            if pred(v) {
                return true;
            }
        }
        false
    }

    pub fn peek_n_expect(&self, steps: usize, pred: fn(&T) -> bool) -> bool {
        if let Some(v) = self.buffer.get(steps) {
            if pred(v) {
                return true;
            }
        }
        false
    }

    /// peeks at the next element and steps forward if it matches,
    /// returns true if a matching element was found
    pub fn peek_and_step_if(&mut self, pred: T) -> bool
    where
        T: PartialEq,
    {
        match self.take_if(pred) {
            Some(_) => true,
            None => false,
        }
    }

    pub fn peek_and_step_if_fn(&mut self, pred: fn(&T) -> bool) -> bool {
        match self.take_if_fn(pred) {
            Some(_) => true,
            None => false,
        }
    }

    /// takes the elemnt at the front and returns it
    pub fn take(&mut self) -> Option<T> {
        let v = self.buffer.pop_front();
        if let Some(ref ch) = v {
            self.step_index(&Some(ch.clone()));
        }
        v
    }

    /// takes a element of the stream,
    /// returns Ok if there is an element
    /// Err if not
    pub fn take_or<E>(&mut self, err: E) -> Result<T, E> {
        if let Some(x) = self.take() {
            return Ok(x);
        };
        return Err(err);
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
        if let Some(v) = self.buffer.get(0) {
            if *v == pred {
                return self.take();
            }
        }
        None
    }
    pub fn take_if_fn(&mut self, pred: fn(&T) -> bool) -> Option<T> {
        if let Some(v) = self.buffer.get(0) {
            if pred(v) {
                return self.take();
            }
        }
        None
    }

    pub fn take_expecting<U, E>(&mut self, pred: fn(T) -> Result<U, E>) -> Result<U, E> {
        let Some(v) = self.take() else {
            // TODO: should not panic?
            panic!("take_expecting -> no more tokens in stream")
        };

        pred(v)
    }

    pub fn take_until_iter(&mut self, pred: fn(&T) -> bool) -> impl Iterator<Item = T> + '_ {
        StreamTakeIterator {
            stream: self,
            pred,
            invert_pred: true,
        }
    }

    pub fn take_until(&mut self, pred: fn(&T) -> bool) -> Vec<T> {
        self.take_until_iter(pred).collect()
    }

    /// takes item while the predicate is true
    pub fn take_while_iter(&mut self, pred: fn(&T) -> bool) -> impl Iterator<Item = T> + '_ {
        StreamTakeIterator {
            stream: self,
            pred,
            invert_pred: false,
        }
    }

    /// see [take_while_iter()]
    pub fn take_while(&mut self, pred: fn(&T) -> bool) -> Vec<T> {
        self.take_while_iter(pred).collect()
    }

    pub fn step_index(&mut self, x: &Option<T>) {
        let Some(y) = x else {
            return;
        };
        if T::is_line_separator(y) {
            self.index.step_row()
        } else {
            self.index.step_col()
        }
    }
}

impl<T> From<Vec<T>> for Stream<T>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    T: Default,
    T: LineSeparator<Item = T>,
{
    fn from(value: Vec<T>) -> Self {
        Self {
            buffer: VecDeque::from(value),
            index: SourceIndex::default(),
        }
    }
}

struct StreamTakeIterator<'a, T>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    T: LineSeparator<Item = T>,
{
    stream: &'a mut Stream<T>,
    pred: fn(&T) -> bool,
    invert_pred: bool,
}

impl<T> Iterator for StreamTakeIterator<'_, T>
where
    T: Clone,
    T: PartialEq,
    T: std::fmt::Debug,
    T: LineSeparator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.stream.buffer.get(0) {
            if self.invert_pred {
                if !(self.pred)(x) {
                    return self.stream.take();
                }
            } else {
                if (self.pred)(x) {
                    return self.stream.take();
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

    impl LineSeparator for TestWrapper {
        type Item = TestWrapper;

        fn is_line_separator(x: &Self::Item) -> bool {
            x.value == -1
        }
    }

    fn generate_test_stream(len: i32) -> Stream<TestWrapper> {
        Stream::from(
            (1..=len)
                .map(|x| TestWrapper { value: x })
                .collect::<Vec<TestWrapper>>(),
        )
    }

    #[test]
    fn from_vec() {
        let s = generate_test_stream(10);
        assert_eq!(10, s.buffer.len());
    }

    #[test]
    fn take_until() {
        let mut s = generate_test_stream(10);
        let t = s.take_until_iter(|x| x.value == 3).last();
        assert_eq!(Some(TestWrapper { value: 2 }), t);
    }

    #[test]
    fn take_while() {
        let mut s = generate_test_stream(10);
        let t = s.take_while_iter(|x| x.value < 3).last();
        assert_eq!(Some(TestWrapper { value: 2 }), t);
    }

    #[test]
    fn peek_expect() {
        let s = generate_test_stream(10);
        assert_eq!(true, s.peek_expect(|x| x.value == 1));
        assert_eq!(false, s.peek_expect(|x| x.value == 2));
    }

    #[test]
    fn peek() {
        let s = generate_test_stream(10);
        assert_eq!(Some(TestWrapper { value: 1 }), s.peek());
    }

    #[test]
    fn peek_n() {
        let s = generate_test_stream(10);
        assert_eq!(Some(TestWrapper { value: 2 }), s.peek_n(1));
    }
    #[test]
    fn peek_n_expect() {
        let s = generate_test_stream(10);
        assert_eq!(true, s.peek_n_expect(1, |x| x.value == 2));
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
