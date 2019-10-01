 %module SwigInterface
 %include "std_string.i"
 %include "std_map.i"
 %include <std_vector.i>

namespace std {
    %template(map_string_string) map<string, string>;
}
void loadInput(const std::map<std::string, std::string> &val, const std::string &prog_ins, const std::string &rel_name, const std::string &rel_name_out);

%{
#include "SwigInterface.h"
#include <iostream>
#include <string>
#include <map>
#include <vector>
using namespace std;
souffle::SouffleProgram *prog;
souffle::Relation *rel;
souffle::Relation *rel_out; 

void
loadInput(const map<string, string> &val, const string &prog_ins, const string &rel_name, const string &rel_name_out)
{
    prog = souffle::ProgramFactory::newInstance(prog_ins); 
    rel = prog->getRelation(rel_name);

    map<string, string>::const_iterator i = val.begin();
    map<string, string>::const_iterator end = val.end();
    cout << "Input \n";
    while (i != end) {
        cout << i->first << " : " << i->second << endl;
        souffle::tuple t(rel);
        t << i->first << i->second;
        rel->insert(t);      
        ++i;
    }
prog->run();

rel_out = prog->getRelation(rel_name_out);
cout << "Output \n";
            // iterate over output relation 
            for(auto &output : *rel_out ) { 
               std::string src, dest; 

               // retrieve elements from tuple 
               output >> src >> dest;

               // print source and destination node 
               cout << src << " - " << dest << "\n";
            }
 
prog->printAll(".");
}
%}

%include "SwigInterface.h"
%newobject newInstance;
SWIGSouffleProgram *newInstance(const std::string &name);
