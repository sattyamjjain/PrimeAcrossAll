DELIMITER //

CREATE PROCEDURE isPrime(IN num INT, OUT result VARCHAR(20))
BEGIN
    DECLARE i INT DEFAULT 2;
    DECLARE is_prime BOOLEAN DEFAULT TRUE;

    IF num <= 1 THEN
        SET result = 'Not a prime number';
        LEAVE done;
    END IF;

    WHILE i * i <= num DO
        IF num % i = 0 THEN
            SET is_prime = FALSE;
            LEAVE done;
        END IF;
        SET i = i + 1;
    END WHILE;

    IF is_prime THEN
        SET result = 'Prime number';
    ELSE
        SET result = 'Not a prime number';
    END IF;

    done: END;
END //

DELIMITER ;
