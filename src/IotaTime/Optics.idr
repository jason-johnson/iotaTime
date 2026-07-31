module IotaTime.Optics

record Identity (value : Type) where
  constructor MkIdentity
  runIdentity : value

Functor Identity where
  map transform (MkIdentity value) = MkIdentity (transform value)

record Constant (stored : Type) (ignored : Type) where
  constructor MkConstant
  getConstant : stored

Functor (Constant stored) where
  map _ (MkConstant value) = MkConstant value

||| A dependency-free van Laarhoven lens.
public export
0 Lens : (source, target, focus, replacement : Type) -> Type
Lens source target focus replacement =
  {0 effect : Type -> Type} ->
  Functor effect =>
  (focus -> effect replacement) -> source -> effect target

public export
0 Lens' : (whole, part : Type) -> Type
Lens' whole part = Lens whole whole part part

public export
lens : (source -> focus) -> (source -> replacement -> target) -> Lens source target focus replacement
lens getter setter update source = map (setter source) (update (getter source))

public export
view : Lens source target focus replacement -> source -> focus
view optic source = getConstant (optic MkConstant source)

public export
set : Lens source target focus replacement -> replacement -> source -> target
set optic value source = runIdentity (optic (const (MkIdentity value)) source)

public export
over : Lens source target focus replacement -> (focus -> replacement) -> source -> target
over optic transform source = runIdentity (optic (MkIdentity . transform) source)

public export
modify : (focus -> replacement) -> Lens source target focus replacement -> source -> target
modify transform optic = over optic transform

public export
compose : Lens source target focus replacement ->
          Lens focus replacement inner replacementInner ->
          Lens source target inner replacementInner
compose outer inner update = outer (inner update)