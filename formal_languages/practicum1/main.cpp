#include "language.h"
#include <iostream>
#include <map>
#include <cstdlib>

int main(int argc, char** argv)
{
    if (argc != 4)
    {
        std::cerr << "Некорректное число аргументов" << std::endl;
        return 0;
    }

    int i = 0;
    char current = 0;
    char x = argv[2][0];
    int k = atoi(argv[3]);

    if (x != 'a' && x != 'b' && x!= 'c')
    {
        std::cerr << "Некорректное x" << std::endl;
        return 0;
    }

    Language language;

    while ((current = argv[1][i]) != '\0')
    {
        bool flag = 0; // встретили ли мы знакомый символ

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
            flag = 1;
        }
        if (current == '1')
        {
            language.emptyWord();
            flag = 1;
        }
        if (current == '.')
        {
            if (!language.concatenation()) return 0;
            flag = 1;
        }
        if (current == '*')
        {
            if (!language.star(k)) return 0;
            flag = 1;
        }
        if (current == '+')
        {
            if (!language.plus()) return 0;
            flag = 1;
        }

        if (!flag)
        {
            std::cerr << "Посторонний символ" << std::endl;
            return 0;
        }

        i++;
    }

    short answer = language.answer(k);
    if (answer == -1)
    {
        std::cerr << "Выражение некорректно" << std::endl;
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
