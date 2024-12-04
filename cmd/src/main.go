package main
import ("fmt")

type Symbol string;

//represents types of arithmetic extressions
type ExprC interface {
    isExprC()
}
//represents a number
type NumC struct {
	n int
}
func (numc NumC) isExprC() {}

// represents application form
type AppC struct {
	fun ExprC
	arg []ExprC
}
func (appc AppC) isExprC() {}

// represents idC
type IdC struct {
	name Symbol
}
func (idc IdC) isExprC() {}

// represents a string
type StrC struct {
	str string
}
func (strc StrC) isExprC() {}

// represents an if statement - 1st expression is the condition, 2nd evaluates if true,
// 3rd evaluates if false
type IfC struct {
	cond ExprC
	t ExprC
	f ExprC
}
func (ifc IfC) isExprC() {}

// represents an anonymous function
type LamC struct {
	args []Symbol
	body ExprC
}
func (lamc LamC) isExprC() {}


type Value interface {
    isValue()
}
type NumV struct {
	val int
}
func (numv NumV) isValue() {}

type BoolV struct {
	val bool
}
func (boolv BoolV) isValue() {}

type StrV struct {
	val string
}
func (strv StrV) isValue() {}

type PrimV struct {
	val Symbol
}
func (PrimV PrimV) isValue() {}

type Binding struct {
	name Symbol
	val Value
}

type Env map[Symbol]Value

func interp(exp ExprC, env Env) {
	
}

func main() {
	topenv := Env{
		"+": PrimV{val: "+"},
		"-": PrimV{val: "-"},
		"*": PrimV{val: "*"},
		"/": PrimV{val: "/"},
		"<=": PrimV{val: "<="},
		"equal?": PrimV{val: "equal?"},
		"true": PrimV{val: "true"},
		"false": PrimV{val: "false"},
		"error": PrimV{val: "error"},
		"println": PrimV{val: "println"},
		"read-num": PrimV{val: "read-num"},
		"read-str": PrimV{val: "read-str"},
		"seq": PrimV{val: "seq"},
		"++": PrimV{val: "++"},
		"random": PrimV{val: "random"},
	}


}