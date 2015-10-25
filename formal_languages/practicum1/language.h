#include <stack>

class Language
{

public:

    Language();
    ~Language();

    void xsymbol();
    void notxSymbol();
    void emptyWord();
    bool concatenation();
    bool star(int k);
    bool plus();
    short answer(int k);
    bool checkStack();

private:

    std::stack< std::pair<int, int> > stack; // <s,l>, s - суффикс, l - длина

};
