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
| 3.1  | Constant folding & PH opt.    |  X  |
| 3.2  | Dead code elimination         |  X  |
| 3.3  | Proc inl & rem uncalls        |  X  |

#### Please write a short (max 500 words) description of the design choices you made. Feel free to reference code, include snippets, ...
In general, I tried to make my design as compact as I could. Recycling most functions when using feval and beval and call and uncall. Therefore most of the functions use additional boolean argument called "forward" which indicates whether the function was passed during forward or backward evaluation. This dictates the behavior of the function accordingly. That way I've shorten the length of my code but on the downside there is some intertwined logic when it comes to uncalls from beval (double reversion).

One of the things that I wasn't sure about was whether to save the list of procedures as global variable or pass them forward as the argument. In the end I've decided to go with passing them as the argument to increase clarity and explicitness of what data is used in every function. Also using to many global variables is usually not the best practice.

For program inverter, I've decided to invert all procedures and don't invert Calls and Uncalls. This way forward passing of inverted_program with starting store of forward evaluation of program gives back the initial store. Looking at it now perhaps it would more compact to only invert the last procedure but now it's already working as it should. :O

When doing optimizations I've struggled a bit when trying to implement basic arithmetic properties (associative & commutative laws) since the program is by default already in kinda hiarchical order so I would probably had to pool values and variables in a sort of list and check what can be done to optimize and apply mentioned rules. I've ran out of time to implement everything to the end, still I've tried to implement as much as I could. But there is surely room for improvement.

#### Consider leaving some feedback about the assignment: what was fun, hard, easy, confusing, ... 

In general I think it's a proper assignment in terms of length and difficulty. Since before this project I didn't really use any programming languages like OCaml I've struggled a lot at the beginning in terms of how to start and how my project should look and what kind of structures should I make. After getting through the initial part it was a fun and challenging project. For people like me perhaps there could be a little bit more guidance at the beginning, e.g. feval function could be a little bit more outlined (what kind of functions you will need, how should the structure kinda look), just so that you can dive in faster. When you come to beval you don't need this kind of guidance anymore but before it could be useful. At least in my opinion.


#### Finally... Good luck with the exam and the rest of your thesis! Happy holidays!