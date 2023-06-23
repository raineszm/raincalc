
# Raincalc

Raincalc is a simple stack-based Reverse Polish Notation (RPN) calculator.

## Installation

Raincalc can be installed by cloning this repo and building with cabal

```bash
git clone git@github.com:raineszm/raincalc.git
cd raincalc
cabal install
```

## Running Tests

To run tests, run the following command

```bash
cabal v2-tests
```

## Usage/Examples


Numbers add themselves to the stack while operators manipulate the numbers
on the stack.

The top of the stack is popped by the operators `.` and the full stack printed by `.s`.
```
raincalc[s:0]> 1 2 3 4 5 6 sum .
21.0
raincalc[s:0]> 3 2 * 2 + .
8.0
raincalc[s:0]> 4 3 2 1 .s
STACK:
(TOP)
0 : 1.0
1 : 2.0
2 : 3.0
3 : 4.0

raincalc[s:4]> prod .
24.0
```
Help information can be obtained by using the `.?` command
```
raincalc[s:0]> .?
Available Commands:

! * ** + - / 1/ ^ abs acos acosh asin asinh atan atanh c ch cos cosh deg e exp
fact factorial inv ln log neg pi prod rad s sgn sh sign signum sin sinh
sqrt sum t tan tanh th todegrees toradians v ||

. .q .quit .r .reg .s clear dup q quit registers swap

-> .? .h .help <- ? x:

raincalc[s:0]> .? .?
Help for .?:
Displays help information
raincalc[s:0]> .? neg
Help for neg:
[... x] -> [... neg(x)]
raincalc[s:0]>
```

## License

[MIT](https://choosealicense.com/licenses/mit/)

## See Also

For a similar tool written in clojure see [cljrpn](https://github.com/raineszm/cljrpn).
