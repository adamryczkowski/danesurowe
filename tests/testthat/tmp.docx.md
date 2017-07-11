% Adam  Ryczkowski
% Variable validation report
% Sun Jul  2 15:04:46 2017




+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Variable name                                                                  | Problems                                                                         |
+================================================================================+==================================================================================+
| character variable Zmienna tekstowa (`txt`)  with formula `syntax error soft`  | * doesn't support tagged NA. Removing all tagged NA into plain NA. Consider re-  |
|                                                                                | placing the vector into numeric                                                  |
|                                                                                | * has the following syntax error in its formula: *<text>:1:8: unexpected symbol  |
|                                                                                | 1: syntax error                                                                  |
|                                                                                | ^*.                                                                              |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| character variable Zmienna liczbowa z brakami (`num1`)  with formula `U-       | * doesn't support tagged NA. Removing all tagged NA into plain NA. Consider re-  |
| npaired(((`                                                                    | placing the vector into numeric                                                  |
|                                                                                | * should be integer but in fact is of type(s) character. contains case that is   |
|                                                                                | nonnumeric: WAW3: nas                                                            |
|                                                                                | * has the following syntax error in its formula: *<text>:2:0: unexpected end of  |
|                                                                                | input                                                                            |
|                                                                                | 1: Unpaired(((                                                                   |
|                                                                                | ^*.                                                                              |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna liczbowa bez braków (`num2`)  with formula `23`                        | * has no Excel counterpart for the R formula                                     |
|                                                                                | * has a formula that produces incompatible object of type `numeric`, where type  |
|                                                                                | `integer` was expected                                                           |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| numeric variable Faktor z brakami (`fac1`)  with formula `unknownvar>0`        | * doesn't support tagged NAs. Promoting integer into the numeric                 |
|                                                                                | * should be factor but in fact is of type(s) numeric.                            |
|                                                                                | * has a formula that refers to the following udefined symbols: `unknownvar`      |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna chyba numeric (`fac2`)  with formula `unknownvar>unkn3`                | * should be integer but in fact is of type(s) character. contains case that is   |
|                                                                                | nonnumeric: VIE1: NF                                                             |
|                                                                                | * has a formula that refers to the following udefined symbols: `unknownvar` and  |
|                                                                                | `unkn3`                                                                          |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z max i min teor (`num_maxmin`)                                        | contains 2 cases with value smaller than theoretical min: VIE1: -32 and LAB3: -  |
|                                                                                | 10, and 3 cases with value greater than theoretical max: WAW1: 43, WAW2: 52 and  |
|                                                                                | JAN2: 10                                                                         |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Faktor bez braków, ale z wartościami spoza zakresu (`fac4`)                    | should be factor but in fact is of type(s) integer.                              |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| numeric variable Zmienna ciągła, ale dyskretna (`numdiscr`)                    | contains 2 cases with value that is not integer: WAW1: 0.14 and LAB3: 0.98       |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Taka moja data (`dat2`)                                                        | should be Date but in fact is of type(s) numeric.                                |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| dwupoziomowa2 (`dobule2`)  with formula `double*2+pi`                          | * should be labelled but in fact is of type(s) numeric.                          |
|                                                                                | * has no Excel counterpart for the R formula                                     |
|                                                                                | * has a formula that evaluates with the following error: *non-numeric argument   |
|                                                                                | to binary operator*                                                              |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z walidacją (`validacja1`)  with formula `double*2 + double2 + 1`      | * has a formula that refers to the following udefined symbols: `double2`         |
|                                                                                | * Must be even in the following 3 cases: WAW2: 6, WAW3: 8 and JAN1: 6            |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| My new variable (`new_var`)                                                    | should be labelled but in fact is of type(s) numeric.                            |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| numeric variable Zmienna liczbowa bez braków ale z NA (`num4`)                 | doesn't support tagged NAs. Promoting integer into the numeric                   |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| numeric variable Zmienna required (`required1`)                                | * doesn't support tagged NAs. Promoting integer into the numeric                 |
|                                                                                | * contains 2 missing data in the following cases: VIE1 and LAB2                  |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z wartością losową 1 (`form1`)  with formula `form1 * 2`               | has a formula that produces incompatible object of type `numeric`, where type    |
|                                                                                | `integer` was expected                                                           |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z wartością losową 2 (`form2`)  with formula `form3 + sin(form4)`      | has a formula that produces incompatible object of type `numeric`, where type    |
|                                                                                | `integer` was expected                                                           |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z wartością losową 3 (`form3`)  with formula `form4 * 2`               | has a formula that produces incompatible object of type `numeric`, where type    |
|                                                                                | `integer` was expected                                                           |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna z wartością losową 4 (`form4`)  with formula `form1 + 1`               | has a formula that produces incompatible object of type `numeric`, where type    |
|                                                                                | `integer` was expected                                                           |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna wyliczana zwracajaca cos dziwnego1 (`form_err_ret1`)  with formula     | * has no Excel counterpart for the R formula                                     |
| `c(2,3,4)`                                                                     | * has a formula that produces vector of size 3 where size 9 was expected         |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Zmienna wyliczana zwracajaca cos dziwnego2 (`form_err_ret2`)  with formula `a- | * has no Excel counterpart for the R formula                                     |
| s.Date("19990909")`                                                            | * has a formula that produces incompatible object of type `Date`, where type `i- |
|                                                                                | nteger` was expected                                                             |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+
| Suma (`form5`)  with formula `form1+form2+form3+form4`                         | has a formula that evaluates to a vector with1 values different from existing    |
|                                                                                | values "Suma" (form5)                                                            |
+--------------------------------------------------------------------------------+----------------------------------------------------------------------------------+

Table: Problems with the dataframe 


