#include <iostream>
#include <map>
using namespace std;

class Exp {
public:
  virtual int interpret() = 0;
};

class Zero : public Exp {

public:

  int interpret () { 
    return 0;
  }

};

class Succ : public Exp {

private:

  Exp *x;

public:

  Succ (Exp *y) {
    x = y;
  }
 
  int interpret () {
    return (x -> interpret()) + 1;
  }

};


class Then : public Exp {

private:

  Exp *x;
  Exp *y;

public:

  Then (Exp *v, Exp *w) {
    x = v;
    y = w;
  }
 
  int interpret () {
    x -> interpret();
    return (y -> interpret()); 
  }

};


// Define a map for the state
map<string, int, less<string> > State;


class Var : public Exp {

private:

  string *i;

public:

  Var (string *v) {
    i = v;
  }
 
  int interpret () {
    return State[*i];
  }

};


class Assign : public Exp {

private:

  string *l;
  Exp    *r;

public:

  Assign (string *v, Exp *w) {
    l = v;
    r = w;
  }
 
  int interpret () {
    State[*l] = r->interpret();
  }

};


int main (void) {
  string a("a");
  cout << ( new Then( new Assign(&a, new Succ(new Zero()))
	  , new Then( new Assign(&a, new Succ(new Var(&a)))
		    , new Succ(new Var(&a))
          ))) -> interpret()
       << endl;
}
