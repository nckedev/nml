use std::collections::VecDeque;

pub struct Stream<T> {
    buffer: VecDeque<T>,
}

impl<T> Stream<T>
where
    T: Copy,
{
    /// Returns the next value without moving forward forward in the stream
    pub fn peek(&self) -> Option<T> {
        if let Some(v) = self.buffer.get(0) {
            return Some(*v);
        }
        None
    }

    /// matches the next value in the stream
    pub fn peek_match(&self, pred: T) -> bool
    where
        T: PartialEq,
    {
        match self.peek() {
            Some(v) if v == pred => true,
            _ => false,
        }
    }

    /// peeks n steps ahead and returns the value without moving forward in the stream
    pub fn peek_n(&self, steps: usize) -> Option<T> {
        if let Some(v) = self.buffer.get(steps) {
            return Some(*v);
        }
        None
    }
    pub fn peek_n_expect(&self, steps: usize, pred: fn(&T) -> bool) -> bool {
        if let Some(v) = self.buffer.get(steps) {
            if pred(v) {
                return true;
            }
        }
        false
    }

    pub fn peek_expect(&self, pred: fn(&T) -> bool) -> bool {
        if let Some(v) = self.buffer.get(0) {
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
        self.buffer.pop_front()
    }

    pub fn take_if(&mut self, pred: T) -> Option<T>
    where
        T: PartialEq,
    {
        if let Some(v) = self.buffer.get(0) {
            if *v == pred {
                return self.buffer.pop_front();
            }
        }
        None
    }
    pub fn take_if_fn(&mut self, pred: fn(&T) -> bool) -> Option<T> {
        if let Some(v) = self.buffer.get(0) {
            if pred(v) {
                return self.buffer.pop_front();
            }
        }
        None
    }

    pub fn take_until_iter(&mut self, pred: fn(&T) -> bool) -> impl Iterator<Item = T> + '_ {
        StreamTakeIterator {
            buffer: &mut self.buffer,
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
            buffer: &mut self.buffer,
            pred,
            invert_pred: false,
        }
    }

    /// see [take_while_iter()]
    pub fn take_while(&mut self, pred: fn(&T) -> bool) -> Vec<T> {
        self.take_while_iter(pred).collect()
    }
}

impl<T> From<Vec<T>> for Stream<T> {
    fn from(value: Vec<T>) -> Self {
        Self {
            buffer: VecDeque::from(value),
        }
    }
}

struct StreamTakeIterator<'a, T> {
    buffer: &'a mut VecDeque<T>,
    pred: fn(&T) -> bool,
    invert_pred: bool,
}

impl<T> Iterator for StreamTakeIterator<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.buffer.get(0) {
            if self.invert_pred {
                if !(self.pred)(x) {
                    return self.buffer.pop_front();
                }
            } else {
                if (self.pred)(x) {
                    return self.buffer.pop_front();
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_vec() {
        let s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        assert_eq!(6, s.buffer.len());
    }

    #[test]
    fn take_until() {
        let mut s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        let t = s.take_until_iter(|&x| x == 3).last();
        assert_eq!(Some(2), t);
    }

    #[test]
    fn take_while() {
        let mut s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        let t = s.take_while_iter(|&x| x < 3).last();
        assert_eq!(Some(2), t);
    }

    #[test]
    fn peek_expect() {
        let s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        assert_eq!(true, s.peek_expect(|&x| x == 1));
        assert_eq!(false, s.peek_expect(|&x| x == 2));
    }

    #[test]
    fn peek() {
        let s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        assert_eq!(Some(1), s.peek());
    }

    #[test]
    fn peek_n() {
        let s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        assert_eq!(Some(2), s.peek_n(1));
    }
    #[test]
    fn peek_n_expect() {
        let s = Stream::from(vec![1, 2, 3, 4, 5, 6]);
        assert_eq!(true, s.peek_n_expect(1, |x| *x == 2));
    }

    #[test]
    fn take() {
        let mut s = Stream::from(vec![1, 2]);
        assert_eq!(Some(1), s.take());
        assert_eq!(Some(2), s.take());
        assert_eq!(None, s.take());
    }

    #[test]
    fn take_if() {
        let mut s = Stream::from(vec![1, 2]);
        assert_eq!(None, s.take_if_fn(|&x| x == 2));
        assert_eq!(Some(1), s.take());
        assert_eq!(Some(2), s.take_if_fn(|&x| x == 2));
    }
}
