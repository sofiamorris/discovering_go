package main
import ("fmt")
import "errors"

type Symbol string;

//represents types of arithmetic extressions
type ExprC interface {
    isExprC()
}
//represents a number
type NumC struct {
	num int
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

type ErrorV struct {}
func (err ErrorV) isValue() {}

type Binding struct {
	name Symbol
	val Value
}

type Env map[Symbol]Value

func interp(exp ExprC, env Env) (Value, error) {
	var ret Value
	var err error = nil
	switch e := exp.(type) {
	case NumC:
		ret=NumV{val: e.num}
	case IdC:
		val, ok := env[e.name]
		if ok {
			ret=val
		} else {
			ret=ErrorV{}
			err=errors.New("id not in env")
		}
	case StrC:
		ret=StrV{val: e.str}
	case IfC:
		var interped Value
		interped,err = interp(e.cond, env)
		switch v := interped.(type) {
		case BoolV: 
			if v.val {
				interp(e.t, env)
			} else {
				interp(e.f, env)
			}
		default: 
			ret=ErrorV{}
			err=errors.New("condition must evaluate to boolean")
		}

	case AppC:
		//
	case LamC:
		//
	}
	return ret, err
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
	exp := NumC{num: 3}
	interp(exp, topenv)

}