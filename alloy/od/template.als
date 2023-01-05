///////////////////////////////////////////////////
// Generic Head of CD Model - adapted/simplified
///////////////////////////////////////////////////

//Names of fields/associations in classes of the model
abstract sig FName {}

//Parent of all classes relating fields and values
abstract sig Obj {
  get : FName -> set Obj
}

pred ObjFNames[objs : set Obj, fNames : set FName] {
  no objs.get[FName - fNames]
}

pred ObjLUAttrib[objs : set Obj, fName : FName, fType : set Obj, low, up : Int] {
  ObjLAttrib[objs, fName, fType, low]
  all o : objs | #o.get[fName] =< up
}

pred ObjLAttrib[objs : set Obj, fName : FName, fType : set Obj, low : Int] {
  objs.get[fName] in fType
  all o : objs | #o.get[fName] >= low
}

pred ObjLU[objs : set Obj, fName : FName, fType : set Obj, low, up : Int] {
  ObjL[objs, fName, fType, low]
  ObjU[objs, fName, fType, up]
}

pred ObjL[objs : set Obj, fName : FName, fType : set Obj, low : Int] {
  all r : objs | #{l : fType | r in l.get[fName]} >= low
}

pred ObjU[objs : set Obj, fName : FName, fType : set Obj, up : Int] {
  all r : objs | #{l : fType | r in l.get[fName]} =< up
}

pred Composition[left : set Obj, lFName : set FName, right : set Obj] {
  // all l1, l2 : left | (#{l1.get[lFName] & l2.get[lFName]} > 0) => l1 = l2
  all r : right | #{l : left, lF : lFName | r in l.get[lF]} =< 1
}
