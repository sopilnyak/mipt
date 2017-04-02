#include "language.h"
#include <iostream>
#include <map>

int main(int argc, char** argv)
{
    if (argc != 4)
    {
        throw std::invalid_argument("Incorrect number of arguments");
    }

    int i = 0;
    char current = 0;
    char x = argv[2][0];
    int k = atoi(argv[3]);

    if (x != 'a' && x != 'b' && x!= 'c')
    {
        throw std::invalid_argument("Incorrect x");
    }

    Language language;

    while ((current = argv[1][i]) != '\0')
    {
        bool isCharacterUnknown = 0;

        if (current == 'a' || current == 'b' || current == 'c')
        {
            if (current == x)
            {
                language.xsymbol();
            }
            else
            {
                language.notxSymbol();
            }
            isCharacterUnknown = 1;
        }
        if (current == '1')
        {
            language.emptyWord();
            isCharacterUnknown = 1;
        }
        if (current == '.')
        {
            if (!language.concatenation())
            {
                return 0;
            }
            isCharacterUnknown = 1;
        }
        if (current == '*')
        {
            if (!language.star(k))
            {
                return 0;
            }
            isCharacterUnknown = 1;
        }
        if (current == '+')
        {
            if (!language.plus())
            {
                return 0;
            }
            isCharacterUnknown = 1;
        }

        if (!isCharacterUnknown)
        {
            throw std::invalid_argument("Wrong symbol");
        }

        i++;
    }

    short answer = language.answer(k);
    if (answer == -1)
    {
        throw std::invalid_argument("Expression is incorrect");
    }
    if (answer == 0)
    {
        std::cout << "NO" << std::endl;
    }
    if (answer == 1)
    {
        std::cout << "YES" << std::endl;
    }

    return 0;
}