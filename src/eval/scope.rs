use std::rc::Rc;
use std::cell::{RefCell, Ref};
use std::hash::{Hash, Hasher};

use eval::Value;

#[derive(Debug, Clone)]
pub struct MutableScope(RefCell<Scope>);

impl MutableScope {
    pub fn new(scope: Scope) -> Self {
        MutableScope(RefCell::new(scope))
    }

    pub fn set(&self, scope: Scope) {
        *self.0.borrow_mut() = scope;
    }

    pub fn borrow(&self) -> Ref<Scope> {
        self.0.borrow()
    }
}

impl Hash for MutableScope {
    fn hash<T: Hasher>(&self, hasher: &mut T) {
        (Rc::as_ref(&self.0.borrow().0) as *const _).hash(hasher)
    }
}

impl PartialEq for MutableScope {
    fn eq(&self, other: &Self) -> bool {
        *self.0.borrow() == *other.0.borrow()
    }
}

impl Eq for MutableScope { }

#[derive(Debug, Clone)]
pub struct Scope(Rc<OwnedScope>);

impl Scope {
    pub fn get(&self, name: &str) -> Option<Rc<Value>> {
        self.0.get(name)
    }

    pub fn parent(&self) -> Option<Scope> {
        self.0.parent.clone()
    }

    pub fn new_with_var(name: String, value: Rc<Value>) -> Scope {
        Rc::new(
            OwnedScope {
                parent: None,
                name: name,
                value: value,
            }
        ).into()
    }

    pub fn with_var(&self, name: String, value: Rc<Value>) -> Scope {
        Rc::new(
            OwnedScope {
                parent: Some(self.clone()),
                name: name,
                value: value,
            }
        ).into()
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        (Rc::as_ref(&self.0) as *const _) == (Rc::as_ref(&other.0) as *const _)
    }
}

impl Eq for Scope { }

impl From<Rc<OwnedScope>> for Scope {
    fn from(scope: Rc<OwnedScope>) -> Self {
        Scope(scope)
    }
}

#[derive(Debug, Clone)]
struct OwnedScope {
    parent: Option<Scope>,
    name: String,
    value: Rc<Value>,
}

impl OwnedScope {
    pub fn get(&self, key: &str) -> Option<Rc<Value>> {
        if self.name == key {
            Some(self.value.clone())
        } else {
            if let Some(ref p) = self.parent {
                p.get(key)
            } else {
                None
            }
        }
    }
}

