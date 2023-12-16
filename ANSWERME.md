## Please fill this file in before submitting


| NAME                  | R NUMBER            | EMAIL ADDRESS |
|-----------------------|---------------------|---------------|
| Lenart Rupnik         | R-0977429           |rupnik.lenart@student.kuleuven.be    |

#### Please mark the assignments that you wish to submit for grading with an 'X', following the example of 'Example assignment': 

| Sect | Assignment                    | Sub |
|------|-------------------------------|-----|
| N/A  | Example assignment            |  X  |
| 2.1  | Basic control flow            |  X  |
| 2.2  | Procedures                    |  X  |
| 2.3  | Loops                         |  X  |
| 2.4  | Backwards evaluation          |  X  |
| 2.5  | Program inversion             |  X  |
| 3.1  | Constant folding & PH opt.    |     |
| 3.2  | Dead code elimination         |     |
| 3.3  | Proc inl & rem uncalls        |     |

#### Please write a short (max 500 words) description of the design choices you made. Feel free to reference code, include snippets, ...
In general, I tried to make my design as compact as I could. Recycling most functions when using feval and beval and call and uncall. So most of the functions use additional boolean argument called "forward" which indicates whether the function was passed during forward or backward evaluation. This dictates the behaviour of the function accordingly. That way I've shorten the length of my code but on the downside there is some intertwined logic when it comes to uncalls from beval (double reversion).

One of the things that I wasn't sure about was whether to save the list of procedures as global variable or pass them forward as the argument. In the end I've decided to go with passing them as the argument to increase clarity and explicitnes of what data is used in every function. Also using to many global variables is usually not the best practise.

For program inverter, I've decided to invert all procedures and don't invert Calls and Uncalls. This way forward passing of inverted_program with starting store of forward evaluation of program gives back the initial store.

#### Consider leaving some feedback about the assignment: what was fun, hard, easy, confusing, ... 

<CHANGEME>

#### Finally... Good luck with the exam and the rest of your thesis! Happy holidays!