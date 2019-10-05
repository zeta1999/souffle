
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
struct t_btree_2__0_1__3 {
using t_tuple = Tuple<RamDomain, 2>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_2__0_1__3& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t) const {
context h;
return equalRange_3(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
struct t_btree_2__0_1__1__3 {
using t_tuple = Tuple<RamDomain, 2>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1>>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_2__0_1__1__3& other) {
ind_0.insertAll(other.ind_0);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> equalRange_0(const t_tuple& t, context& h) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> equalRange_0(const t_tuple& t) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t, context& h) const {
t_tuple low(t); t_tuple high(t);
low[1] = MIN_RAM_DOMAIN;
high[1] = MAX_RAM_DOMAIN;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_3(const t_tuple& t) const {
context h;
return equalRange_3(t, h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printHintStatistics(std::ostream& o, const std::string prefix) const {
const auto& stats_0 = ind_0.getHintStatistics();
o << prefix << "arity 2 direct b-tree index [0,1]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};

class Sf_family : public SouffleProgram {
private:
static inline bool regex_wrapper(const std::string& pattern, const std::string& text) {
   bool result = false; 
   try { result = std::regex_match(text, std::regex(pattern)); } catch(...) { 
     std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\"" << text << "\").\n";
}
   return result;
}
private:
static inline std::string substr_wrapper(const std::string& str, size_t idx, size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
private:
static inline RamDomain wrapper_tonumber(const std::string& str) {
   RamDomain result=0; 
   try { result = stord(str); } catch(...) { 
     std::cerr << "error: wrong string provided by to_number(\"";
     std::cerr << str << "\") functor.\n";
     raise(SIGFPE);
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable
{
	R"_(tom)_",
	R"_(amy)_",
	R"_(adam)_",
	R"_(jack)_",
	R"_(fred)_",
	R"_(tony)_",
	R"_(carolII)_",
	R"_(carolIII)_",
	R"_(graceI)_",
	R"_(carolI)_",
};// -- Table: father
std::unique_ptr<t_btree_2__0_1__3> rel_1_father = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<0,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_1_father;
// -- Table: mother
std::unique_ptr<t_btree_2__0_1__3> rel_2_mother = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<1,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_2_mother;
// -- Table: parent
std::unique_ptr<t_btree_2__0_1__1__3> rel_3_parent = std::make_unique<t_btree_2__0_1__1__3>();
souffle::RelationWrapper<2,t_btree_2__0_1__1__3,Tuple<RamDomain,2>,2> wrapper_rel_3_parent;
// -- Table: ancestor
std::unique_ptr<t_btree_2__0_1__1__3> rel_4_ancestor = std::make_unique<t_btree_2__0_1__1__3>();
souffle::RelationWrapper<3,t_btree_2__0_1__1__3,Tuple<RamDomain,2>,2> wrapper_rel_4_ancestor;
// -- Table: @delta_ancestor
std::unique_ptr<t_btree_2__0_1__1__3> rel_5_delta_ancestor = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: @new_ancestor
std::unique_ptr<t_btree_2__0_1__1__3> rel_6_new_ancestor = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: grandmother
std::unique_ptr<t_btree_2__0_1__3> rel_7_grandmother = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<4,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_7_grandmother;
// -- Table: sibling
std::unique_ptr<t_btree_2__0_1__3> rel_8_sibling = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<5,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_8_sibling;
// -- Table: cousin
std::unique_ptr<t_btree_2__0_1__3> rel_9_cousin = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<6,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_9_cousin;
// -- Table: relative
std::unique_ptr<t_btree_2__0_1__3> rel_10_relative = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<7,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_10_relative;
public:
Sf_family() : 
wrapper_rel_1_father(*rel_1_father,symTable,"father",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_2_mother(*rel_2_mother,symTable,"mother",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_3_parent(*rel_3_parent,symTable,"parent",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_4_ancestor(*rel_4_ancestor,symTable,"ancestor",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_7_grandmother(*rel_7_grandmother,symTable,"grandmother",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_8_sibling(*rel_8_sibling,symTable,"sibling",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_9_cousin(*rel_9_cousin,symTable,"cousin",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_10_relative(*rel_10_relative,symTable,"relative",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}){
addRelation("father",&wrapper_rel_1_father,false,false);
addRelation("mother",&wrapper_rel_2_mother,false,false);
addRelation("parent",&wrapper_rel_3_parent,false,true);
addRelation("ancestor",&wrapper_rel_4_ancestor,false,true);
addRelation("grandmother",&wrapper_rel_7_grandmother,false,true);
addRelation("sibling",&wrapper_rel_8_sibling,false,true);
addRelation("cousin",&wrapper_rel_9_cousin,false,true);
addRelation("relative",&wrapper_rel_10_relative,false,true);
}
~Sf_family() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
SignalHandler::instance()->setMsg(R"_(father("tom","amy").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [33:1-33:21])_");
rel_1_father->insert(RamDomain(0),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(father("tom","adam").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [34:1-34:22])_");
rel_1_father->insert(RamDomain(0),RamDomain(2));
SignalHandler::instance()->setMsg(R"_(father("jack","fred").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [35:1-35:23])_");
rel_1_father->insert(RamDomain(3),RamDomain(4));
SignalHandler::instance()->setMsg(R"_(father("tony","carolII").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [36:1-36:26])_");
rel_1_father->insert(RamDomain(5),RamDomain(6));
SignalHandler::instance()->setMsg(R"_(father("fred","carolIII").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [37:1-37:27])_");
rel_1_father->insert(RamDomain(4),RamDomain(7));
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
SignalHandler::instance()->setMsg(R"_(mother("graceI","amy").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [38:1-38:24])_");
rel_2_mother->insert(RamDomain(8),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(mother("amy","fred").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [39:1-39:22])_");
rel_2_mother->insert(RamDomain(1),RamDomain(4));
SignalHandler::instance()->setMsg(R"_(mother("carolI","carolII").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [40:1-40:28])_");
rel_2_mother->insert(RamDomain(9),RamDomain(6));
SignalHandler::instance()->setMsg(R"_(mother("carolII","carolIII").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [41:1-41:30])_");
rel_2_mother->insert(RamDomain(6),RamDomain(7));
}();
/* END STRATUM 1 */
/* BEGIN STRATUM 2 */
[&]() {
SignalHandler::instance()->setMsg(R"_(parent(X,Y) :- 
   father(X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [19:1-19:28])_");
if(!(rel_1_father->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_1_father_op_ctxt,rel_1_father->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_1_father) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_3_parent->insert(tuple,READ_OP_CONTEXT(rel_3_parent_op_ctxt));
}
}
();}
SignalHandler::instance()->setMsg(R"_(parent(X,Y) :- 
   mother(X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [20:1-20:28])_");
if(!(rel_2_mother->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
CREATE_OP_CONTEXT(rel_2_mother_op_ctxt,rel_2_mother->createContext());
for(const auto& env0 : *rel_2_mother) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_3_parent->insert(tuple,READ_OP_CONTEXT(rel_3_parent_op_ctxt));
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./parent.csv"},{"name","parent"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_3_parent);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_1_father->purge();
}();
/* END STRATUM 2 */
/* BEGIN STRATUM 3 */
[&]() {
SignalHandler::instance()->setMsg(R"_(ancestor(X,Y) :- 
   parent(X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [22:1-22:30])_");
if(!(rel_3_parent->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_ancestor_op_ctxt,rel_4_ancestor->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_3_parent) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_4_ancestor->insert(tuple,READ_OP_CONTEXT(rel_4_ancestor_op_ctxt));
}
}
();}
rel_5_delta_ancestor->insertAll(*rel_4_ancestor);
iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(ancestor(X,Y) :- 
   parent(X,Z),
   ancestor(Z,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [23:1-23:45])_");
if(!(rel_5_delta_ancestor->empty()) && !(rel_3_parent->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_delta_ancestor_op_ctxt,rel_5_delta_ancestor->createContext());
CREATE_OP_CONTEXT(rel_6_new_ancestor_op_ctxt,rel_6_new_ancestor->createContext());
CREATE_OP_CONTEXT(rel_4_ancestor_op_ctxt,rel_4_ancestor->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_3_parent) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_5_delta_ancestor->equalRange_1(key,READ_OP_CONTEXT(rel_5_delta_ancestor_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_4_ancestor->contains(Tuple<RamDomain,2>({{env0[0],env1[1]}}),READ_OP_CONTEXT(rel_4_ancestor_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_6_new_ancestor->insert(tuple,READ_OP_CONTEXT(rel_6_new_ancestor_op_ctxt));
}
}
}
}
();}
if(rel_6_new_ancestor->empty()) break;
rel_4_ancestor->insertAll(*rel_6_new_ancestor);
std::swap(rel_5_delta_ancestor, rel_6_new_ancestor);
rel_6_new_ancestor->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled()) rel_5_delta_ancestor->purge();
if (!isHintsProfilingEnabled()) rel_6_new_ancestor->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./ancestor.csv"},{"name","ancestor"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_ancestor);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}();
/* END STRATUM 3 */
/* BEGIN STRATUM 4 */
[&]() {
SignalHandler::instance()->setMsg(R"_(grandmother(X,Y) :- 
   mother(X,Z),
   ancestor(Z,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [25:1-25:48])_");
if(!(rel_4_ancestor->empty()) && !(rel_2_mother->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_grandmother_op_ctxt,rel_7_grandmother->createContext());
CREATE_OP_CONTEXT(rel_4_ancestor_op_ctxt,rel_4_ancestor->createContext());
CREATE_OP_CONTEXT(rel_2_mother_op_ctxt,rel_2_mother->createContext());
for(const auto& env0 : *rel_2_mother) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_4_ancestor->equalRange_1(key,READ_OP_CONTEXT(rel_4_ancestor_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_7_grandmother->insert(tuple,READ_OP_CONTEXT(rel_7_grandmother_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./grandmother.csv"},{"name","grandmother"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_7_grandmother);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_2_mother->purge();
}();
/* END STRATUM 4 */
/* BEGIN STRATUM 5 */
[&]() {
SignalHandler::instance()->setMsg(R"_(sibling(X,Y) :- 
   parent(Z,X),
   parent(Z,Y),
   X != Y.
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [27:1-27:50])_");
if(!(rel_3_parent->empty()) && !(rel_3_parent->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_sibling_op_ctxt,rel_8_sibling->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_3_parent) {
const Tuple<RamDomain,2> key({{env0[0],0}});
auto range = rel_3_parent->equalRange_1(key,READ_OP_CONTEXT(rel_3_parent_op_ctxt));
for(const auto& env1 : range) {
if( ((env0[1]) != (env1[1]))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env1[1])}});
rel_8_sibling->insert(tuple,READ_OP_CONTEXT(rel_8_sibling_op_ctxt));
}
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./sibling.csv"},{"name","sibling"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_sibling);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}();
/* END STRATUM 5 */
/* BEGIN STRATUM 6 */
[&]() {
SignalHandler::instance()->setMsg(R"_(cousin(X,Y) :- 
   ancestor(Z,X),
   ancestor(Z,Y),
   !sibling(X,Y),
   !parent(X,Y),
   X != Y.
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [29:1-29:82])_");
if(!(rel_4_ancestor->empty()) && !(rel_4_ancestor->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_8_sibling_op_ctxt,rel_8_sibling->createContext());
CREATE_OP_CONTEXT(rel_9_cousin_op_ctxt,rel_9_cousin->createContext());
CREATE_OP_CONTEXT(rel_4_ancestor_op_ctxt,rel_4_ancestor->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_4_ancestor) {
const Tuple<RamDomain,2> key({{env0[0],0}});
auto range = rel_4_ancestor->equalRange_1(key,READ_OP_CONTEXT(rel_4_ancestor_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_8_sibling->contains(Tuple<RamDomain,2>({{env0[1],env1[1]}}),READ_OP_CONTEXT(rel_8_sibling_op_ctxt))) && !(rel_3_parent->contains(Tuple<RamDomain,2>({{env0[1],env1[1]}}),READ_OP_CONTEXT(rel_3_parent_op_ctxt))) && ((env0[1]) != (env1[1]))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env1[1])}});
rel_9_cousin->insert(tuple,READ_OP_CONTEXT(rel_9_cousin_op_ctxt));
}
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./cousin.csv"},{"name","cousin"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_9_cousin);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_4_ancestor->purge();
}();
/* END STRATUM 6 */
/* BEGIN STRATUM 7 */
[&]() {
SignalHandler::instance()->setMsg(R"_(relative(X,Y) :- 
   sibling(X,Z),
   parent(Z,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/family/family.dl [31:1-31:43])_");
if(!(rel_3_parent->empty()) && !(rel_8_sibling->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_10_relative_op_ctxt,rel_10_relative->createContext());
CREATE_OP_CONTEXT(rel_8_sibling_op_ctxt,rel_8_sibling->createContext());
CREATE_OP_CONTEXT(rel_3_parent_op_ctxt,rel_3_parent->createContext());
for(const auto& env0 : *rel_8_sibling) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_3_parent->equalRange_1(key,READ_OP_CONTEXT(rel_3_parent_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_10_relative->insert(tuple,READ_OP_CONTEXT(rel_10_relative_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./relative.csv"},{"name","relative"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_10_relative);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_3_parent->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_8_sibling->purge();
}();
/* END STRATUM 7 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_father:\n";
rel_1_father->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_mother:\n";
rel_2_mother->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_parent:\n";
rel_3_parent->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_ancestor:\n";
rel_4_ancestor->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_5_delta_ancestor:\n";
rel_5_delta_ancestor->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_6_new_ancestor:\n";
rel_6_new_ancestor->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_7_grandmother:\n";
rel_7_grandmother->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_8_sibling:\n";
rel_8_sibling->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_9_cousin:\n";
rel_9_cousin->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_10_relative:\n";
rel_10_relative->printHintStatistics(std::cout,"  ");
std::cout << "\n";
}
SignalHandler::instance()->reset();
}
public:
void run(size_t stratumIndex = (size_t) -1) override { runFunction(".", ".", stratumIndex, false); }
public:
void runAll(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1) override { runFunction(inputDirectory, outputDirectory, stratumIndex, true);
}
public:
void printAll(std::string outputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./parent.csv"},{"name","parent"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_3_parent);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./ancestor.csv"},{"name","ancestor"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_ancestor);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./grandmother.csv"},{"name","grandmother"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_7_grandmother);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./sibling.csv"},{"name","sibling"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_sibling);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./cousin.csv"},{"name","cousin"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_9_cousin);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./relative.csv"},{"name","relative"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_10_relative);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = ".") override {
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_3_parent");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_3_parent);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_4_ancestor");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_ancestor);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_7_grandmother");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_7_grandmother);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_8_sibling");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_sibling);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_9_cousin");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_9_cousin);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_10_relative");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_10_relative);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
};
SouffleProgram *newInstance_family(){return new Sf_family;}
SymbolTable *getST_family(SouffleProgram *p){return &reinterpret_cast<Sf_family*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_family: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_family();
};
public:
factory_Sf_family() : ProgramFactory("family"){}
};
static factory_Sf_family __factory_Sf_family_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(swig/java/family/family.dl)",
R"(.)",
R"(.)",
false,
R"()",
1,
-1);
if (!opt.parse(argc,argv)) return 1;
#if defined(_OPENMP) 
omp_set_nested(true);

#endif
souffle::Sf_family obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
