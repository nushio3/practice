#include <fstream>
#include <iostream>
#include <map>
#include <unordered_map>

using namespace std;

typedef long long int ident_t;


ident_t new_free_ident (){
  static ident_t global_free_ident = 0;
  return ++global_free_ident;
}

enum Operator {
  Unk, Imm,  Add, Sub
};

struct expr {
  expr () : op(Unk) {}
  expr (Operator op, float lit) : op(op), lit(lit) {}
  expr (Operator op, ident_t lhs, ident_t rhs) : op(op), lhs(lhs), rhs(rhs) {}
  Operator op;
  ident_t lhs;
  ident_t rhs;
  float lit;
};

map<ident_t, expr> global_binding;
map<string, ident_t> global_trace_ident;
map<ident_t, string> global_trace_label;

void bind(ident_t i, expr e) {
  global_binding[i]=e;
}

struct traced_float {
  ident_t ident;

  traced_float () {
    ident = new_free_ident();
    bind(ident,expr());
  }
  traced_float (ident_t ident) : ident(ident) {}
  traced_float (const float lit) {
    ident=new_free_ident();
    bind(ident,expr(Imm,lit));

  }
  traced_float operator+(const traced_float &other){
    traced_float ret(new_free_ident());
    bind(ret.ident,expr(Add,this->ident,other.ident));
    return ret;
  };
  traced_float operator-(const traced_float &other){
    traced_float ret(new_free_ident());
    bind(ret.ident,expr(Sub,this->ident,other.ident));
    return ret;
  };
};

ostream& operator<<(ostream& ostr, const traced_float &x) {
  return ostr << "<traced>";
}

#ifdef TRACE
void trace(string label, const traced_float &x) {
  global_trace_ident[label] = x.ident;
  global_trace_label[x.ident] = label;
}
string varname_of_ident(ident_t ident) {
  if(global_trace_label.count(ident)) {
    return global_trace_label[ident];
  }
  return string("x") + to_string(ident);
}

#ifdef MODEL_IEEE_FLOAT
const bool model_ieee_float = true;
string freeVarFunc = "sFloat";
#else
const bool model_ieee_float = false;
string freeVarFunc = "sReal";
#endif


void prove_equality(string label_a, string label_b) {
  cerr << "Is " << label_a << " == " << label_b << "?" << endl;
  ofstream ofs("body.hs");
  typedef map<ident_t, expr> bm;
  string indent = "  ";
  for (bm::iterator it = global_binding.begin(); it != global_binding.end(); ++it) {
    string v1 = varname_of_ident(it->first);
    ofs << indent << v1 << " <- " << freeVarFunc << " \"" << v1 << "\"" << endl;
    const expr &x = it->second;
    switch (x.op) {
    case Unk:
      if (model_ieee_float)
        ofs << indent << "constrain $  isNormalFP " << v1 << endl;
      break;
    case Imm:
      ofs << indent << "constrain $  " << v1 << ".==" << x.lit << endl;
      break;
    case Add:
      ofs << indent << "constrain $ " << v1 << ".==" << varname_of_ident(x.lhs) << "+" << varname_of_ident(x.rhs) << endl;
      break;
    case Sub:
      ofs << indent << "constrain $ " << v1 << ".==" << varname_of_ident(x.lhs) << "-" << varname_of_ident(x.rhs) << endl;
      break;
    }
  }
  ofs << indent << "return $ " << label_a << ".==" << label_b << endl;
}
#else
template<class t> void trace(string label, t a) { /*does nothing*/ }
template<class t> void prove_equality(t a, t b) { /*does nothing*/ }
#endif
