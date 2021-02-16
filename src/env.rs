use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<T>
where T: Clone {
    curr: HashMap<String, T>,
    parent: Option<Rc<RefCell<Environment<T>>>>,
}

impl<T> Environment<T>
where T: Clone {
    pub fn new() -> Environment<T> {
        Environment {
            curr: HashMap::new(),
            parent: None,
        }
    }

    pub fn make_child(parent: Rc<RefCell<Environment<T>>>) -> Environment<T> {
        Environment {
            curr: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, key: &str) -> Option<T> {
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

    pub fn set(&mut self, key: String, item: T) {
        self.curr.insert(key, item);
    }
}

#[derive(Clone, Debug)]
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
