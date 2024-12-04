package main

import (
	"errors"
	"fmt"
)

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

type CloV struct {
	args []Symbol
	body ExprC
	env Env
}
func (clov CloV) isValue() {}

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
		funVal, err := interp(e.fun, env)
        if err != nil {
            return ErrorV{}, err
        }
        
        // Evaluate all arguments
        argVals := make([]Value, len(e.arg))
        for i, arg := range e.arg {
            val, err := interp(arg, env)
            if err != nil {
                return ErrorV{}, err
            }
            argVals[i] = val
        }
        
        switch funV := funVal.(type) {
        case PrimV:
            return interpPrim(funV.val, argVals)
            
        case CloV:
            if len(funV.args) != len(argVals) {
                return ErrorV{}, errors.New("AAQZ arity mismatch between params and args")
            }
            
            // Create new environment with function arguments
            newEnv := make(Env)
            for k, v := range funV.env {
                newEnv[k] = v
            }
            
            // Bind arguments to parameters in new environment
            for i, param := range funV.args {
                newEnv[param] = argVals[i]
            }
            
            return interp(funV.body, newEnv)
            
        default:
            return ErrorV{}, errors.New("AAQZ cannot apply non-function value")
        }
	case LamC:
		ret=CloV{
			args: e.args,
			body: e.body,
			env: env,
		}
	}
	return ret, err
}

func interpPrim(op Symbol, args []Value) (Value, error) {
    switch op {
    case "+", "-", "*", "/":
        if len(args) != 2 {
            return ErrorV{}, errors.New("AAQZ arithmetic operations require exactly two arguments")
        }
        
        n1, ok1 := args[0].(NumV)
        n2, ok2 := args[1].(NumV)
        if !ok1 || !ok2 {
            return ErrorV{}, errors.New("AAQZ arithmetic operations require numeric arguments")
        }
        
        switch op {
        case "+":
            return NumV{val: n1.val + n2.val}, nil
        case "-":
            return NumV{val: n1.val - n2.val}, nil
        case "*":
            return NumV{val: n1.val * n2.val}, nil
        case "/":
            if n2.val == 0 {
                return ErrorV{}, errors.New("AAQZ division by zero")
            }
            return NumV{val: n1.val / n2.val}, nil
        }
        
    case "<=":
        if len(args) != 2 {
            return ErrorV{}, errors.New("AAQZ comparison requires exactly two arguments")
        }
        
        n1, ok1 := args[0].(NumV)
        n2, ok2 := args[1].(NumV)
        if !ok1 || !ok2 {
            return ErrorV{}, errors.New("AAQZ comparison requires numeric arguments")
        }
        
        return BoolV{val: n1.val <= n2.val}, nil
        
    case "equal?":
        if len(args) != 2 {
            return ErrorV{}, errors.New("AAQZ equal? requires exactly two arguments")
        }
        return BoolV{val: fmt.Sprint(args[0]) == fmt.Sprint(args[1])}, nil
        
    case "println":
        if len(args) != 1 {
            return ErrorV{}, errors.New("AAQZ println requires exactly one argument")
        }
        
        str, ok := args[0].(StrV)
        if !ok {
            return ErrorV{}, errors.New("AAQZ println requires string argument")
        }
        
        fmt.Println(str.val)
        return BoolV{val: true}, nil
        
    case "seq":
        if len(args) == 0 {
            return ErrorV{}, errors.New("AAQZ seq requires at least one expression")
        }
        return args[len(args)-1], nil
        
    case "++":
        if len(args) == 0 {
            return ErrorV{}, errors.New("AAQZ ++ requires at least one argument")
        }
        
        result := ""
        for _, arg := range args {
            switch v := arg.(type) {
            case StrV:
                result += v.val
            case NumV:
                result += fmt.Sprint(v.val)
            default:
                result += fmt.Sprint(v)
            }
        }
        return StrV{val: result}, nil
        
    default:
        return ErrorV{}, errors.New("AAQZ unknown primitive operation: " + string(op))
    }
    return ErrorV{}, errors.New("AAQZ unhandled primitive operation")
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
	}
	exp := NumC{num: 3}
	interp(exp, topenv)

}