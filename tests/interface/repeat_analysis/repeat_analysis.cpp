
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
struct t_btree_2__0_1__1 {
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
void insertAll(t_btree_2__0_1__1& other) {
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
struct t_btree_1__0 {
using t_tuple = Tuple<RamDomain, 1>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0>>;
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
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_1__0& other) {
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
o << prefix << "arity 1 direct b-tree index [0]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};
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
struct t_btree_2__0_1 {
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
void insertAll(t_btree_2__0_1& other) {
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
struct t_btree_1__0__1 {
using t_tuple = Tuple<RamDomain, 1>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0>>;
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
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_1__0__1& other) {
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
range<t_ind_0::iterator> equalRange_1(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
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
o << prefix << "arity 1 direct b-tree index [0]: (hits/misses/total)\n";
o << prefix << "Insert: " << stats_0.inserts.getHits() << "/" << stats_0.inserts.getMisses() << "/" << stats_0.inserts.getAccesses() << "\n";
o << prefix << "Contains: " << stats_0.contains.getHits() << "/" << stats_0.contains.getMisses() << "/" << stats_0.contains.getAccesses() << "\n";
o << prefix << "Lower-bound: " << stats_0.lower_bound.getHits() << "/" << stats_0.lower_bound.getMisses() << "/" << stats_0.lower_bound.getAccesses() << "\n";
o << prefix << "Upper-bound: " << stats_0.upper_bound.getHits() << "/" << stats_0.upper_bound.getMisses() << "/" << stats_0.upper_bound.getAccesses() << "\n";
}
};

class Sf_repeat_analysis : public SouffleProgram {
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
;// -- Table: edge
std::unique_ptr<t_btree_2__0_1__1> rel_1_edge = std::make_unique<t_btree_2__0_1__1>();
souffle::RelationWrapper<0,t_btree_2__0_1__1,Tuple<RamDomain,2>,2,true,false> wrapper_rel_1_edge;
// -- Table: source
std::unique_ptr<t_btree_1__0> rel_2_source = std::make_unique<t_btree_1__0>();
souffle::RelationWrapper<1,t_btree_1__0,Tuple<RamDomain,1>,1,true,false> wrapper_rel_2_source;
// -- Table: path_from_source
std::unique_ptr<t_btree_2__0_1__3> rel_3_path_from_source = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<2,t_btree_2__0_1__3,Tuple<RamDomain,2>,2,false,false> wrapper_rel_3_path_from_source;
// -- Table: @delta_path_from_source
std::unique_ptr<t_btree_2__0_1> rel_4_delta_path_from_source = std::make_unique<t_btree_2__0_1>();
// -- Table: @new_path_from_source
std::unique_ptr<t_btree_2__0_1> rel_5_new_path_from_source = std::make_unique<t_btree_2__0_1>();
// -- Table: sink
std::unique_ptr<t_btree_1__0__1> rel_6_sink = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<3,t_btree_1__0__1,Tuple<RamDomain,1>,1,true,false> wrapper_rel_6_sink;
// -- Table: source2sink
std::unique_ptr<t_btree_2__0_1> rel_7_source2sink = std::make_unique<t_btree_2__0_1>();
souffle::RelationWrapper<4,t_btree_2__0_1,Tuple<RamDomain,2>,2,false,true> wrapper_rel_7_source2sink;
public:
Sf_repeat_analysis() : 
wrapper_rel_1_edge(*rel_1_edge,symTable,"edge",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_2_source(*rel_2_source,symTable,"source",std::array<const char *,1>{{"s:Node"}},std::array<const char *,1>{{"node"}}),

wrapper_rel_3_path_from_source(*rel_3_path_from_source,symTable,"path_from_source",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_6_sink(*rel_6_sink,symTable,"sink",std::array<const char *,1>{{"s:Node"}},std::array<const char *,1>{{"node"}}),

wrapper_rel_7_source2sink(*rel_7_source2sink,symTable,"source2sink",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"source","sink"}}){
addRelation("edge",&wrapper_rel_1_edge,1,0);
addRelation("source",&wrapper_rel_2_source,1,0);
addRelation("path_from_source",&wrapper_rel_3_path_from_source,0,0);
addRelation("sink",&wrapper_rel_6_sink,1,0);
addRelation("source2sink",&wrapper_rel_7_source2sink,0,1);
}
~Sf_repeat_analysis() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

#if defined(__EMBEDDED_SOUFFLE__) && defined(_OPENMP)
omp_set_num_threads(1);
#endif

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./edge.facts"},{"name","edge"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1, 1}), symTable, ioDirectives, 0)->readAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./source.facts"},{"name","source"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1}), symTable, ioDirectives, 0)->readAll(*rel_2_source);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}();
/* END STRATUM 1 */
/* BEGIN STRATUM 2 */
[&]() {
SignalHandler::instance()->setMsg(R"_(path_from_source(X,Y) :- 
   source(X),
   edge(X,Y).
in file /code/souffle/tests/interface/repeat_analysis/repeat_analysis.dl [12:1-14:16])_");
if (!rel_1_edge->empty()&&!rel_2_source->empty()) [&](){
auto part = rel_2_source->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
CREATE_OP_CONTEXT(rel_3_path_from_source_op_ctxt,rel_3_path_from_source->createContext());
CREATE_OP_CONTEXT(rel_2_source_op_ctxt,rel_2_source->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{for(const auto& env0 : *it) {
const Tuple<RamDomain,2> key({{env0[0],0}});
auto range = rel_1_edge->equalRange_1(key,READ_OP_CONTEXT(rel_1_edge_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_3_path_from_source->insert(tuple,READ_OP_CONTEXT(rel_3_path_from_source_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
();rel_4_delta_path_from_source->insertAll(*rel_3_path_from_source);
iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(path_from_source(X,Z) :- 
   path_from_source(X,Y),
   edge(Y,Z).
in file /code/souffle/tests/interface/repeat_analysis/repeat_analysis.dl [15:1-17:16])_");
if (!rel_4_delta_path_from_source->empty()&&!rel_1_edge->empty()) [&](){
auto part = rel_4_delta_path_from_source->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_4_delta_path_from_source_op_ctxt,rel_4_delta_path_from_source->createContext());
CREATE_OP_CONTEXT(rel_5_new_path_from_source_op_ctxt,rel_5_new_path_from_source->createContext());
CREATE_OP_CONTEXT(rel_1_edge_op_ctxt,rel_1_edge->createContext());
CREATE_OP_CONTEXT(rel_3_path_from_source_op_ctxt,rel_3_path_from_source->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{for(const auto& env0 : *it) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_1_edge->equalRange_1(key,READ_OP_CONTEXT(rel_1_edge_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_3_path_from_source->contains(Tuple<RamDomain,2>({{env0[0],env1[1]}}),READ_OP_CONTEXT(rel_3_path_from_source_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env1[1])}});
rel_5_new_path_from_source->insert(tuple,READ_OP_CONTEXT(rel_5_new_path_from_source_op_ctxt));
}
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
();if(rel_5_new_path_from_source->empty()) break;
rel_3_path_from_source->insertAll(*rel_5_new_path_from_source);
std::swap(rel_4_delta_path_from_source, rel_5_new_path_from_source);
rel_5_new_path_from_source->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled() && (performIO || 1)) rel_4_delta_path_from_source->purge();
if (!isHintsProfilingEnabled() && (performIO || 1)) rel_5_new_path_from_source->purge();
if (!isHintsProfilingEnabled() && (performIO || 0)) rel_1_edge->purge();
if (!isHintsProfilingEnabled() && (performIO || 0)) rel_2_source->purge();
}();
/* END STRATUM 2 */
/* BEGIN STRATUM 3 */
[&]() {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./sink.facts"},{"name","sink"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1}), symTable, ioDirectives, 0)->readAll(*rel_6_sink);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
}();
/* END STRATUM 3 */
/* BEGIN STRATUM 4 */
[&]() {
SignalHandler::instance()->setMsg(R"_(source2sink(Source,Sink) :- 
   path_from_source(Source,Sink),
   sink(Sink).
in file /code/souffle/tests/interface/repeat_analysis/repeat_analysis.dl [22:1-24:17])_");
if (!rel_3_path_from_source->empty()&&!rel_6_sink->empty()) [&](){
auto part = rel_3_path_from_source->partition();
PARALLEL_START;
CREATE_OP_CONTEXT(rel_3_path_from_source_op_ctxt,rel_3_path_from_source->createContext());
CREATE_OP_CONTEXT(rel_6_sink_op_ctxt,rel_6_sink->createContext());
CREATE_OP_CONTEXT(rel_7_source2sink_op_ctxt,rel_7_source2sink->createContext());
pfor(auto it = part.begin(); it<part.end();++it){
try{for(const auto& env0 : *it) {
const Tuple<RamDomain,1> key({{env0[1]}});
auto range = rel_6_sink->equalRange_1(key,READ_OP_CONTEXT(rel_6_sink_op_ctxt));
if(!range.empty()) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_7_source2sink->insert(tuple,READ_OP_CONTEXT(rel_7_source2sink_op_ctxt));
}
}
} catch(std::exception &e) { SignalHandler::instance()->error(e.what());}
}
PARALLEL_END;
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","source\tsink"},{"filename","./source2sink.csv"},{"name","source2sink"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(SymbolMask({1, 1}), symTable, ioDirectives, 0)->writeAll(*rel_7_source2sink);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled() && (performIO || 0)) rel_6_sink->purge();
if (!isHintsProfilingEnabled() && (performIO || 0)) rel_3_path_from_source->purge();
}();
/* END STRATUM 4 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_edge:\n";
rel_1_edge->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_source:\n";
rel_2_source->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_path_from_source:\n";
rel_3_path_from_source->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_delta_path_from_source:\n";
rel_4_delta_path_from_source->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_5_new_path_from_source:\n";
rel_5_new_path_from_source->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_6_sink:\n";
rel_6_sink->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_7_source2sink:\n";
rel_7_source2sink->printHintStatistics(std::cout,"  ");
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","source\tsink"},{"filename","./source2sink.csv"},{"name","source2sink"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(SymbolMask({1, 1}), symTable, ioDirectives, 0)->writeAll(*rel_7_source2sink);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectory = ".") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./edge.facts"},{"name","edge"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1, 1}), symTable, ioDirectives, 0)->readAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./source.facts"},{"name","source"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1}), symTable, ioDirectives, 0)->readAll(*rel_2_source);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"filename","./sink.facts"},{"name","sink"}});
if (!inputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = inputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getReader(SymbolMask({1}), symTable, ioDirectives, 0)->readAll(*rel_6_sink);
} catch (std::exception& e) {std::cerr << "Error loading data: " << e.what() << '\n';}
}
public:
void dumpInputs(std::ostream& out = std::cout) override {
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_1_edge");
IOSystem::getInstance().getWriter(SymbolMask({1, 1}), symTable, ioDirectives, 0)->writeAll(*rel_1_edge);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_2_source");
IOSystem::getInstance().getWriter(SymbolMask({1}), symTable, ioDirectives, 0)->writeAll(*rel_2_source);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_6_sink");
IOSystem::getInstance().getWriter(SymbolMask({1}), symTable, ioDirectives, 0)->writeAll(*rel_6_sink);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs(std::ostream& out = std::cout) override {
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_7_source2sink");
IOSystem::getInstance().getWriter(SymbolMask({1, 1}), symTable, ioDirectives, 0)->writeAll(*rel_7_source2sink);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
const SymbolTable &getSymbolTable() const override {
return symTable;
}
};
SouffleProgram *newInstance_repeat_analysis(){return new Sf_repeat_analysis;}
SymbolTable *getST_repeat_analysis(SouffleProgram *p){return &reinterpret_cast<Sf_repeat_analysis*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_repeat_analysis: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_repeat_analysis();
};
public:
factory_Sf_repeat_analysis() : ProgramFactory("repeat_analysis"){}
};
static factory_Sf_repeat_analysis __factory_Sf_repeat_analysis_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(repeat_analysis.dl)",
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
souffle::Sf_repeat_analysis obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
