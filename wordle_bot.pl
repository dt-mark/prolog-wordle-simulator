:- dynamic data/2, generationWords/1, guessWords/1, generatedWord/1, guessCount/1, orangeLetter/2, greenLetter/2, greyLetter/1, wordFrequency/2, wordScore/2, gen/2.

% ----------------------------------------------------------------------------------------------------------------- %
% Global predicates ----------------------------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
assert_once(Fact) :- \+(Fact), !, assert(Fact).
assert_once(_).
isGenWord(Word) :- gen(Word, _).
isGuessWord(Word) :- wordFrequency(Word, _).
isRelevantWord(Word) :- wordScore(Word, _).
increment_count :- guessCount(Count), retractall(guessCount(_)), NewCount is Count + 1, assert(guessCount(NewCount)).
update_possible_guesses_list(List) :- retractall(guessWords(_)), assert(guessWords(List)).

% ----------------------------------------------------------------------------------------------------------------- %
% Import databases into the program ------------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
import :- 
    retractall(generationWords(_)), retractall(guessWords(_)), retractall(data(_, _)),
    cd('C:/Users/DT Mark/Documents/Game Development/Projects/Wordle Bot/prolog-wordle-simulator'),
    csv_read_file('Words/generation_table.csv', GenerationRows, [functor(gen), arity(2)]), maplist(assert, GenerationRows),
    findall(Word, isGenWord(Word), GenerationWords), assert(generationWords(GenerationWords)),
    retractall(gen(_, _)),
    csv_read_file('Words/guess_table.csv', Rows, [functor(wordFrequency), arity(2)]), maplist(assert, Rows),
    findall(Word, isGuessWord(Word), GuessWords), assert(guessWords(GuessWords)).

% ----------------------------------------------------------------------------------------------------------------- %
% Export data to csv file ----------------------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
save :-
    generatedWord(GeneratedWord), guessCount(Count),
    assert(data(GeneratedWord, Count)).

map_item(P, row(C1, C2)) :- P = [C1, C2].
export :- 
    get_time(TimeStamp), format_time(string(Time), '%d%m%Y_%H%M%S', TimeStamp), 
    string_concat("Output/output_", Time, OutFileName1), string_concat(OutFileName1, ".csv", OutFileName), 
    string_to_atom(OutFileName, OutFile),
    findall([C1, C2], data(C1, C2), Data),
    maplist(map_item, Data, Rows),
    csv_write_file(OutFile, Rows, []).

% ----------------------------------------------------------------------------------------------------------------- %
% Launch a batch of games and export them ------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
launch(0) :- 
    export.
launch(N) :-
    play,
    save,
    NextN is N - 1,
    launch(NextN). 

% ----------------------------------------------------------------------------------------------------------------- %
% Initialise a game by resetting global variables, generating a word, and launching guesses ----------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
play :- 
    retractall(generatedWord(_)), retractall(guessCount(_)), 
    retractall(orangeLetter(_, _)), retractall(greenLetter(_, _)), retractall(greyLetter(_)),
    retractall(wordScore(_, _)), retractall(guessWords(_)),
    findall(Word, isGuessWord(Word), GuessWords), assert(guessWords(GuessWords)),
    assert(guessCount(0)),
    generationWords(GenerationWords), random_member(GeneratedWord, GenerationWords), !, assert(generatedWord(GeneratedWord)),
    write('-----'), nl, write(GeneratedWord), nl, write('-----'), nl,
    guess, !.

% ----------------------------------------------------------------------------------------------------------------- %
% Sort words by score --------------------------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
sort_words_by_score(Words, SortedWords) :-
    map_list_to_pairs(wordScore, Words, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, SortedWords1),
    reverse(SortedWords1, SortedWords).

% ----------------------------------------------------------------------------------------------------------------- %
% Choose a word from list of possible words, increment count, and evaluate the chosen word ------------------------ %
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
    write(GuessWord), nl,
    evaluate_guess(GuessWord, GeneratedWord, Status),
    (Status == unfinished -> guess ; true).

% ----------------------------------------------------------------------------------------------------------------- %
% Check if guess word is the same as generated word, otherwise update hint database and word scores --------------- %
% ----------------------------------------------------------------------------------------------------------------- %
evaluate_guess(GuessWord, GuessWord, finished).
evaluate_guess(GuessWord, GeneratedWord, unfinished) :-
    GuessWord \== GeneratedWord,
    atom_string(GuessWord, GuessWS), string_to_list(GuessWS, GuessWordList),
    atom_string(GeneratedWord, GeneratedWS), string_to_list(GeneratedWS, GeneratedWordList),
    update_orange_hints(GuessWordList, GeneratedWordList, 0, GeneratedWordList),
    update_green_hints(GuessWordList, GeneratedWordList, 0, GeneratedWordList),
    update_grey_hints(GuessWordList, GeneratedWordList),
    update_word_scores.

% ----------------------------------------------------------------------------------------------------------------- %
% Update orange hint letter database based on the comparison between the guess and the generated word ------------- %
% ----------------------------------------------------------------------------------------------------------------- %
update_orange_hints([X], [X], _, _).
update_orange_hints([X], [Y], I, FullWord) :- 
    X \== Y, 
    (member(X, FullWord) -> assert_once(orangeLetter(X, I)) ; true).
update_orange_hints([X|R1], [X|R2], I, FullWord) :- 
    NextI is I + 1, 
    update_orange_hints(R1, R2, NextI, FullWord).
update_orange_hints([X|R1], [Y|R2], I, FullWord) :- 
    X \== Y, 
    (member(X, FullWord) -> assert_once(orangeLetter(X, I)) ; true),  
    NextI is I + 1, 
    update_orange_hints(R1, R2, NextI, FullWord).

% ----------------------------------------------------------------------------------------------------------------- %
% Update green hint letter database based on the comparison between the guess and the generated word -------------- %
% ----------------------------------------------------------------------------------------------------------------- %
update_green_hints([X], [X], I, _) :- 
    assert_once(greenLetter(X, I)).
update_green_hints([X], [Y], _, _) :- 
    X \== Y.
update_green_hints([X|R1], [X|R2], I, FullWord) :- 
    assert_once(greenLetter(X, I)), 
    NextI is I + 1, 
    update_green_hints(R1, R2, NextI, FullWord).
update_green_hints([X|R1], [Y|R2], I, FullWord) :- 
    X \== Y, 
    NextI is I + 1, 
    update_green_hints(R1, R2, NextI, FullWord).
    
% ----------------------------------------------------------------------------------------------------------------- %
% Update grey hint letter database based on the comparison between the guess and the generated word --------------- %
% ----------------------------------------------------------------------------------------------------------------- %
update_grey_hints(GuessWordList, GeneratedWordList) :-
    list_to_set(GuessWordList, GuessWordSet),
    subtract(GuessWordSet, GeneratedWordList, GreyLetters),
    update_grey_hints_1(GreyLetters).
update_grey_hints_1([]).
update_grey_hints_1([X|R]) :-
    assert_once(greyLetter(X)), 
    update_grey_hints_1(R).

% ----------------------------------------------------------------------------------------------------------------- %
% Update and assert scores for every possible word ---------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
update_word_scores :-
    retractall(wordScore(_, _)), 
    guessWords(PossibleWords), 
    update_word_scores_1(PossibleWords),
    findall(Word, isRelevantWord(Word), GuessWords),
    update_possible_guesses_list(GuessWords).
update_word_scores_1([]).
update_word_scores_1([Word|Rest]) :-
    atom_string(Word, WS), string_to_list(WS, WordList),
    computer_letter_score(WordList, 0, 0, LetterScore, Keep),
    (
        Keep == keep -> 
            wordFrequency(Word, Frequency),
            Score is LetterScore + 0.5*Frequency, 
            assert(wordScore(Word, Score)) 
        ; 
            true
    ),
    update_word_scores_1(Rest).

% ----------------------------------------------------------------------------------------------------------------- %
% Compute letter score for a given score -------------------------------------------------------------------------- %
% ----------------------------------------------------------------------------------------------------------------- %
computer_letter_score([], _, Score, Score, keep).
computer_letter_score([Letter|_], I, _, _, nokeep) :-
    orangeLetter(Letter, I).
computer_letter_score([Letter|_], _, _, _, nokeep) :-
    greyLetter(Letter).
computer_letter_score([Letter|Rest], I, Score, NextScore, Keep) :-
    (greenLetter(Letter, I) -> NScore is Score + 10 ; NScore is Score),
    (orangeLetter(Letter, P), I \== P -> NNScore is NScore + 1 ; NNScore is NScore),
    NextI is I + 1,
    computer_letter_score(Rest, NextI, NNScore, NextScore, Keep).
