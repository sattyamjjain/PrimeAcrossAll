%macro is_prime(n);
    %let prime = 1;
    %if &n < 2 %then %let prime = 0;
    %else %do;
        %do i = 2 %to %eval(&n - 1);
            %if %eval(&n % %i) = 0 %then %do;
                %let prime = 0;
                %goto end_loop;
            %end;
        %end;
    %end;
    %end_loop:
    %if &prime = 1 %then %put &n is a prime number.;
    %else %put &n is not a prime number.;
%mend is_prime;

%macro main;
    %put Enter a number to check if it's prime:;
    %let number = 29;  /* Replace this with the input number */
    %is_prime(&number);
%mend main;

%main;
