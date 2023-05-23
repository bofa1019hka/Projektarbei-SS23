#include <iostream>
#include <string>

#include "Compiler.h"

//allgemeine cmd class
class cmd {
private:
    int value;
public:
    cmd(int initialValue) {
        value = initialValue;
    }
    void print() const {
        std::cout << value << std::endl;
    }
    void decrement() {
        value--;
    }
};


int main() {

    Compiler compiler;
    compiler.print2();

    
    cmd* cmd1 = new cmd(1);
    cmd1->print();

    cmd* cmd1b = new cmd(1);
    cmd1b->print();
    cmd1b->decrement();
    cmd1b->print();
    

    

    delete cmd1;
    delete cmd1b;
    return 0;
}