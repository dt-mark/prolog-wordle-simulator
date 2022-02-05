:- dynamic generationWords/1, guessWords/1, generatedWord/1, guessCount/1, orangeLetter/2, greenLetter/2, greyLetter/1, wordFrequency/2, wordScore/2, gen/2.

isGenWord(Word) :- gen(Word, _).
isGuessWord(Word) :- wordFrequency(Word, _).
assert_once(Fact) :- \+(Fact), !, assert(Fact).
assert_once(_).

% ----------------------------------------------------------------------------------------------------------------- %
% Load databases into the program
% ----------------------------------------------------------------------------------------------------------------- %
load :- 
    retractall(generationWords(_)), retractall(guessWords(_)),
    cd('C:/Users/DT Mark/Documents/Game Development/Projects/Wordle Bot'),
    csv_read_file('generation_table.csv', GenerationRows, [functor(gen), arity(2)]), maplist(assert, GenerationRows),
    findall(Word, isGenWord(Word), GenerationWords), assert(generationWords(GenerationWords)),
    retractall(gen(_, _)),
    csv_read_file('guess_table.csv', Rows, [functor(wordFrequency), arity(2)]), maplist(assert, Rows),
    findall(Word, isGuessWord(Word), GuessWords), assert(guessWords(GuessWords)).

% ----------------------------------------------------------------------------------------------------------------- %
% Initialise a game by resetting global variables, generating a word, and launching guesses
% ----------------------------------------------------------------------------------------------------------------- %
launch :- 
    retractall(generatedWord(_)), retractall(guessCount(_)), 
    retractall(orangeLetter(_, _)), retractall(greenLetter(_, _)), retractall(greyLetter(_)),
    retractall(wordScore(_, _)), retractall(guessWords(_)),
    findall(Word, isGuessWord(Word), GuessWords), assert(guessWords(GuessWords)),
    assert(guessCount(0)),
    generationWords(GenerationWords), random_member(GeneratedWord, GenerationWords), !, assert(generatedWord(GeneratedWord)),
    write('-----'), nl, write(GeneratedWord), nl, write('-----'), nl,
    guess.

% ----------------------------------------------------------------------------------------------------------------- %
% Update global variables predicates
% ----------------------------------------------------------------------------------------------------------------- %
increment_count :- guessCount(Count), retractall(guessCount(_)), NewCount is Count + 1, assert(guessCount(NewCount)).
update_possible_guesses_list(List) :- retractall(guessWords(_)), assert(guessWords(List)).

% ----------------------------------------------------------------------------------------------------------------- %
% Sort words by score
% ----------------------------------------------------------------------------------------------------------------- %
sort_words_by_score(Words, SortedWords) :-
    map_list_to_pairs(wordScore, Words, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, SortedWords1),
    reverse(SortedWords1, SortedWords).

% ----------------------------------------------------------------------------------------------------------------- %
% Choose a word from list of possible words, increment count, and evaluate the chosen word
% ----------------------------------------------------------------------------------------------------------------- %
guess :-
    guessWords(PossibleWords), 
    generatedWord(GeneratedWord),
    guessCount(Count),
    (
        Count == 0 -> 
            GuessWord = orate 
        ;
            sort_words_by_score(PossibleWords, SortedPossibleWords),
            nth0(0, SortedPossibleWords, GuessWord)
    ),
    increment_count,
    evaluate_guess(GuessWord, GeneratedWord, Status),
    write(GuessWord), nl,
    (Status == unfinished -> guess ; guessCount(C), write(C), nl).

% ----------------------------------------------------------------------------------------------------------------- %
% Check if guess word is the same as generated word, otherwise compute differences and guess again
% ----------------------------------------------------------------------------------------------------------------- %
evaluate_guess(GuessWord, GuessWord, finished).
evaluate_guess(GuessWord, GeneratedWord, unfinished) :-
    GuessWord \== GeneratedWord,
    atom_string(GuessWord, GuessWS), string_to_list(GuessWS, GuessWordList),
    atom_string(GeneratedWord, GeneratedWS), string_to_list(GeneratedWS, GeneratedWordList),
    update_orange_letters(GuessWordList, GeneratedWordList, 0, GeneratedWordList),
    update_green_letters(GuessWordList, GeneratedWordList, 0, GeneratedWordList),
    update_grey_letters(GuessWordList, GeneratedWordList),
    update_word_scores.

% ----------------------------------------------------------------------------------------------------------------- %
% Update orange hint letter database based on the comparison between the guess and the generated word
% ----------------------------------------------------------------------------------------------------------------- %
update_orange_letters([X], [X], _, _).
update_orange_letters([X], [Y], I, FullWord) :- 
    X \== Y, 
    (member(X, FullWord) -> assert_once(orangeLetter(X, I)); true).
update_orange_letters([X|R1], [X|R2], I, FullWord) :- 
    NextI is I + 1, 
    update_orange_letters(R1, R2, NextI, FullWord).
update_orange_letters([X|R1], [Y|R2], I, FullWord) :- 
    X \== Y, 
    (member(X, FullWord) -> assert_once(orangeLetter(X, I)); true),  
    NextI is I + 1, 
    update_orange_letters(R1, R2, NextI, FullWord).


% ----------------------------------------------------------------------------------------------------------------- %
% Update green hint letter database based on the comparison between the guess and the generated word
% ----------------------------------------------------------------------------------------------------------------- %
update_green_letters([X], [X], I, _) :- 
    assert_once(greenLetter(X, I)).
update_green_letters([X], [Y], _, _) :- 
    X \== Y.
update_green_letters([X|R1], [X|R2], I, FullWord) :- 
    assert_once(greenLetter(X, I)), 
    NextI is I + 1, 
    update_green_letters(R1, R2, NextI, FullWord).
update_green_letters([X|R1], [Y|R2], I, FullWord) :- 
    X \== Y, 
    NextI is I + 1, 
    update_green_letters(R1, R2, NextI, FullWord).
    
% ----------------------------------------------------------------------------------------------------------------- %
% Update grey hint letter database based on the comparison between the guess and the generated word
% ----------------------------------------------------------------------------------------------------------------- %
update_grey_letters(GuessWordList, GeneratedWordList) :-
    list_to_set(GuessWordList, GuessWordSet),
    subtract(GuessWordSet, GeneratedWordList, GreyLetters),
    update_grey_letters_1(GreyLetters).
update_grey_letters_1([]).
update_grey_letters_1([X|R]) :-
    assert_once(greyLetter(X)), 
    update_grey_letters_1(R).

% ----------------------------------------------------------------------------------------------------------------- %
% Update and assert scores for every possible word
% ----------------------------------------------------------------------------------------------------------------- %
update_word_scores :-
    retractall(wordScore(_, _)), 
    guessWords(PossibleWords), 
    update_word_scores_1(PossibleWords),
    exclude([X]>>(wordScore(X, L), L < 0), PossibleWords, FilteredPossibleWords),
    update_possible_guesses_list(FilteredPossibleWords).
update_word_scores_1([]).
update_word_scores_1([Word|Rest]) :-
    evaluate_orange_score(Word, OrangeScore),
    evaluate_green_score(Word, GreenScore),
    evaluate_grey_score(Word, GreyScore),
    wordFrequency(Word, Frequency),
    Score is GreenScore + OrangeScore + GreyScore + 0.5*Frequency,
    assert(wordScore(Word, Score)),
    update_word_scores_1(Rest).

% ----------------------------------------------------------------------------------------------------------------- %
% Evaluate orange score
% ----------------------------------------------------------------------------------------------------------------- %
has_correct_orange(Letter, WordList) :- 
    member(Letter, WordList), 
    nth0(I1, WordList, Letter), 
    orangeLetter(Letter, I2),
    I1 \== I2.
has_incorrect_orange(Letter, WordList) :- 
    member(Letter, WordList), 
    nth0(I, WordList, Letter), 
    orangeLetter(Letter, I).
evaluate_orange_score(Word, Score) :-
    atom_string(Word, WS), string_to_list(WS, WordList), list_to_set(WordList, WordSet),
    findall(L, has_correct_orange(L, WordSet), CorrectOrangeLetters),
    findall(L, has_incorrect_orange(L, WordList), IncorrectOrangeLetters),
    length(CorrectOrangeLetters, PlusScore),
    length(IncorrectOrangeLetters, MinusScore),
    Score is PlusScore - 100 * MinusScore.

% ----------------------------------------------------------------------------------------------------------------- %
% Evaluate green score
% ----------------------------------------------------------------------------------------------------------------- %
has_correct_green(Letter, WordList) :- 
    member(Letter, WordList), 
    nth0(I, WordList, Letter), 
    greenLetter(Letter, I).
evaluate_green_score(Word, Score) :-
    atom_string(Word, WS), string_to_list(WS, WordList), list_to_set(WordList, WordSet),
    findall(L, has_correct_green(L, WordSet), GreenLetters),
    length(GreenLetters, PlusScore),
    Score is 2 * PlusScore.

% ----------------------------------------------------------------------------------------------------------------- %
% Evaluate grey score
% ----------------------------------------------------------------------------------------------------------------- %
has_incorrect_grey(Letter, WordList) :-
    member(Letter, WordList),
    greyLetter(Letter).
evaluate_grey_score(Word, Score) :-
    atom_string(Word, WS), string_to_list(WS, WordList),
    findall(L, has_incorrect_grey(L, WordList), GreyLetters),
    length(GreyLetters, MinusScore),
    Score is -100 * MinusScore.
    
