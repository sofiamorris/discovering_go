package main

import (
	"errors"
	"fmt"
)

type Symbol string

// represents types of arithmetic extressions
type ExprC interface {
	isExprC()
}

// represents a number
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
	t    ExprC
	f    ExprC
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
	env  Env
}

func (clov CloV) isValue() {}

type ErrorV struct{}

func (err ErrorV) isValue() {}

type Env map[Symbol]Value

func interp(exp ExprC, env Env) (Value, error) {
	var ret Value
	var err error = nil
	switch e := exp.(type) {
	case NumC:
		ret = NumV{val: e.num}
	case IdC:
		val, ok := env[e.name]

		if ok {
			ret = val
		} else {
			ret = ErrorV{}
			err = errors.New("AAQZ name not found: " + string(e.name))
		}
	case StrC:
		ret = StrV{val: e.str}
	case IfC:
		var interped Value
		interped, err = interp(e.cond, env)

		if err != nil {
			return interped, err
		}

		switch v := interped.(type) {
		case BoolV:
			if v.val {
				ret, err = interp(e.t, env)
			} else {
				ret, err = interp(e.f, env)
			}
		default:
			ret = ErrorV{}
			err = errors.New("AAQZ expected boolean condition")
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
		ret = CloV{
			args: e.args,
			body: e.body,
			env:  env,
		}
	}
	return ret, err
}

func parse(array interface{}) (ExprC){
	switch v := array.(type){
	case int:
		return NumC{num: v}
	case string:
		return StrC{str: v}
	case Symbol:
		return IdC{name: v}
	case []interface{}:
		if len(v) == 0{
			panic("Invalid expression: empty list")
		}
		head := v[0]

		if head == "if"{
			if len(v) != 4 {
				panic("Invalid 'if' syntax")
			}
			return IfC{cond: parse(v[1]),
				t: parse(v[2]),
				f: parse(v[3])}
		} else if argList, ok := head.([]interface{}); ok {
			if len(v) == 3 && v[1] == "=>"{
				args := make([]Symbol, len(argList))
				for i, arg := range argList {
					argStr, ok := arg.(string)
					if !ok {
						panic(fmt.Sprintf("Invalid lambda argument: %v", arg))
					}
					args[i] = Symbol(argStr)
				}
				body := parse(v[2])
				return LamC{args: args,
					body: body}
			}
		} else {
			funcExpr := parse(v[0])
			args := make([]ExprC, len(v)-1)
			for i, arg:= range v[1:]{
					args[i] = parse(arg)
				}
				return AppC{fun: funcExpr,
					arg: args}
			}
		}
		panic(fmt.Sprintf("Unsupported expression type: %T", array))
	}

//helper function to test parse
func testParse(input interface{}, expected ExprC, error bool){
	result := parse(input)

	if error {
		fmt.Printf("Expected panic but got result: %v", result)
		return
	}

	if fmt.Sprintf("%T", result) != fmt.Sprintf("%T", expected) {
        fmt.Printf("Type mismatch: Expected type %T but got type %T\n", expected, result)
        return
    }
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

func serialize(v Value) (string, error) {
	var result string
	var err error = nil
	switch v := v.(type) {
	case NumV:
		result = fmt.Sprintf("%d", v.val)
	case StrV:
		result = fmt.Sprintf("\"%s\"", v.val)
	case BoolV:
		if v.val {
			result = "true"
		} else {
			result = "false"
		}
	case PrimV:
		result = "#<primop>"
	case CloV:
		result = "#<procedure>"
	default:
		result = "unimplemented"
		err = errors.New("AAQZ serialize: unimplemented Value type")
	}
	return result, err
}

func main() {
	topenv := Env{
		"+":       PrimV{val: "+"},
		"-":       PrimV{val: "-"},
		"*":       PrimV{val: "*"},
		"/":       PrimV{val: "/"},
		"<=":      PrimV{val: "<="},
		"equal?":  PrimV{val: "equal?"},
		"true":    BoolV{val: true},
		"false":   BoolV{val: false},
		"error":   PrimV{val: "error"},
		"println": PrimV{val: "println"},
		"seq":     PrimV{val: "seq"},
		"++":      PrimV{val: "++"},
	}
	// exp := NumC{num: 3}
	exp := IfC{
		cond: AppC{
			fun: IdC{name: "<="},
			arg: []ExprC{NumC{num: 1}, NumC{num: 5}},
		},
		t: NumC{num: 1},
		f: NumC{num: -1},
	}
	v, _ := interp(exp, topenv)
	fmt.Println(serialize(v))

	tests := []struct {
		name     string
		exp      ExprC
		expected Value
	}{
		// Add test cases here:
		{
			name:     "Simple number",
			exp:      NumC{num: 42},
			expected: NumV{val: 42},
		},
		{
			name:     "Simple string",
			exp:      StrC{str: "hello"},
			expected: StrV{val: "hello"},
		},
		{
			name: "If true condition",
			exp: IfC{
				cond: IdC{name: "true"},
				t:    NumC{num: 1},
				f:    NumC{num: 2},
			},
			expected: NumV{val: 1},
		},
	}

	for i, test := range tests {
		result, err := interp(test.exp, topenv)
		if err != nil {
			fmt.Printf("Test %d (%s) failed with error: %v\n", i, test.name, err)
		} else {
			switch expected := test.expected.(type) {
			case NumV:
				if result.(NumV).val != expected.val {
					fmt.Printf("Test %d (%s): got %v, wanted %v\n", i, test.name, result.(NumV).val, expected.val)
				} else {
					fmt.Printf("Test %d (%s): Passed\n", i, test.name)
				}
			case BoolV:
				if result.(BoolV).val != expected.val {
					fmt.Printf("Test %d (%s): got %v, wanted %v\n", i, test.name, result.(BoolV).val, expected.val)
				} else {
					fmt.Printf("Test %d (%s): Passed\n", i, test.name)
				}
			case StrV:
				if result.(StrV).val != expected.val {
					fmt.Printf("Test %d (%s): got %v, wanted %v\n", i, test.name, result.(StrV).val, expected.val)
				} else {
					fmt.Printf("Test %d (%s): Passed\n", i, test.name)
				}
			default:
				fmt.Printf("Test %d (%s): Unexpected type\n", i, test.name)
			}
		}
	}

	testParse(42, NumC{num: 42}, false)
	testParse("hello", StrC{str: "hello"}, false)
	testParse(Symbol("x"), IdC{name: "x"}, false)

	parseIf := []interface{}{"if", "x", "y", "z"}
	expectedIf := IfC{
		cond: IdC{name: "x"},
		t: IdC{name: "y"},
		f: IdC{name: "z"}}
	testParse(parseIf, expectedIf, false)

	parseLambda := []interface{}{
		[]interface{}{"x", "y"},
		"=>",
		[]interface{}{"+", "x", "y"}}
	expectedLambda := LamC{
		args: []Symbol{"x", "y"},
		body: AppC{
			fun: IdC{name: "+"},
			arg: []ExprC{
				IdC{name: "x"},
				IdC{name: "y"}}}}
	testParse(parseLambda, expectedLambda, false)

	parseAppC := []interface{}{
		"+", 
		[]interface{}{1, 2}}
	expectedAppC := AppC{
		fun: IdC{name: "+"},
		arg: []ExprC{
			NumC{num: 1},
			NumC{num: 2}}}
	testParse(parseAppC, expectedAppC, false)
}

