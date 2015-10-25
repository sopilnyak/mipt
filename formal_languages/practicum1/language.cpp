#include "language.h"
#include <iostream>

Language::Language()
{

}

Language::~Language()
{
  while (!stack.empty())
  {
      stack.pop();
  }
}

void Language::xsymbol()
{
    stack.push(std::make_pair(1,1));
}

void Language::notxSymbol()
{
    stack.push(std::make_pair(0,1));
}

void Language::emptyWord()
{
    stack.push(std::make_pair(0,0));
}

bool Language::concatenation()
{
    if (!checkStack()) return 0;
    std::pair<int, int> right = stack.top();
    stack.pop();
    if (!checkStack()) return 0;
    std::pair<int, int> left = stack.top();
    stack.pop();

    if (right.first == right.second)
    {
        stack.push(std::make_pair(right.first + left.first, right.second + left.second));
    }
    else
    {
        stack.push(std::make_pair(right.first, right.second + left.second));
    }
    return 1;
}

bool Language::star(int k)
{
    if (!checkStack()) return 0;
    std::pair<int, int> operand = stack.top();
    stack.pop();

    if (operand.first == operand.second)
    {
        stack.push(std::make_pair(k, k));
    }
    else
    {
        if (!stack.empty() && operand.first < stack.top().first)
        {
            stack.push(std::make_pair(0, 0));
        }
        else
        {
            stack.push(std::make_pair(operand.first, operand.second));
        }
    }
    return 1;
  }

  bool Language::plus()
  {
    if (!checkStack()) return 0;
    std::pair<int, int> right = stack.top();
    stack.pop();
    if (!checkStack()) return 0;
    std::pair<int, int> left = stack.top();
    stack.pop();

    int smax = std::max(right.first, left.first);
    if (smax == right.first)
    {
        stack.push(std::make_pair(right.first, right.second));
    }
    else
    {
        stack.push(std::make_pair(left.first, left.second));
    }
    return 1;
  }

  short Language::answer(int k)
  {
    if (!checkStack()) return -1;

    std::pair<int, int> top = stack.top();
    stack.pop();

    if (!stack.empty())
    {
        return -1;
    }
    if (top.first >= k)
    {
        return 1;
    }
    else
    {
        return 0;
    }
  }

  bool Language::checkStack()
  {
    if (stack.empty())
    {
        std::cerr << "Выражение некорректно" << std::endl;
        return 0;
    }
    return 1;
}
