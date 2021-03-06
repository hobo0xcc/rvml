use std::{collections::HashMap, hash::Hash};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<K, T>
where T: Clone, K: Hash + Eq + Clone {
    curr: HashMap<K, T>,
    parent: Option<Rc<RefCell<Environment<K, T>>>>,
}

impl<K, T> Environment<K, T>
where T: Clone, K: Hash + Eq + Clone {
    pub fn new() -> Environment<K, T> {
        Environment {
            curr: HashMap::new(),
            parent: None,
        }
    }

    pub fn make_child(parent: Rc<RefCell<Environment<K, T>>>) -> Environment<K, T> {
        Environment {
            curr: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, key: &K) -> Option<T> {
        if let Some(item) = self.curr.get(key) {
            return Some(item.clone());
        } else if let Some(parent) = self.parent.clone() {
            return match parent.borrow().get(key) {
                Some(item) => Some(item.clone()),
                None => None,
            };
        } else {
            return None;
        }
    }

    pub fn set(&mut self, key: K, item: T) {
        self.curr.insert(key, item);
    }

    pub fn map_all_elems<R, F: Fn((&K, &T)) -> R>(&self, f: F) -> Vec<R> {
        let mut elems = Vec::new();
        for (key, value) in self.curr.iter() {
            elems.push(f((key, value)));
        }
        if let Some(ref p) = self.parent {
            elems.extend(p.clone().borrow().map_all_elems(f));
        }

        elems
    }

    pub fn map_all_keys<R, F: Fn(&K) -> R>(&self, f: F) -> Vec<R> {
        let mut elems = Vec::new();
        for (key, _) in self.curr.iter() {
            elems.push(f(key));
        }

        if let Some(ref p) = self.parent {
            elems.extend(p.clone().borrow().map_all_keys(f));
        }

        elems
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DeBruijn<T> {
    item: Option<Rc<T>>,
    prev: Option<Rc<DeBruijn<T>>>,
}

impl<T> DeBruijn<T> 
where T: Clone {
    pub fn new() -> DeBruijn<T> {
        DeBruijn {
            item: None,
            prev: None,
        }
    }

    pub fn add(self, item: T) -> DeBruijn<T> {
        let mut new_db = DeBruijn::new();
        new_db.item = Some(Rc::new(item));
        new_db.prev = Some(Rc::new(self));
        return new_db;
    }

    pub fn get_item(self) -> Option<T> {
        match self.item {
            Some(ref rc) => {
                Some((*(rc.clone())).clone())
            },
            None => None,
        }
    }
}

impl<A, B> DeBruijn<(A, B)>
where A: PartialEq + Clone, B: Clone {
    pub fn assoc(&self, a: A) -> Option<B> {
        match self.item {
            Some(_) => {},
            None => return None,
        };
        let i = &self.item;
        match *i {
            Some(ref item) => {
                if (*item).0 == a {
                    return Some(item.1.clone());
                }
            },
            None => unreachable!(),
        }

        match self.prev {
            Some(ref p) => {
                return p.assoc(a)
            },
            None => return None,
        }
    }
}
