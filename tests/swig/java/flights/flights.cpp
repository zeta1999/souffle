
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
struct t_btree_3__0_1_2__1__7 {
using t_tuple = Tuple<RamDomain, 3>;
using t_ind_0 = btree_set<t_tuple, index_utils::comparator<0,1,2>>;
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
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
template <typename T>
void insertAll(T& other) {
for (auto const& cur : other) {
insert(cur);
}
}
void insertAll(t_btree_3__0_1_2__1__7& other) {
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
low[2] = MIN_RAM_DOMAIN;
high[2] = MAX_RAM_DOMAIN;
return make_range(ind_0.lower_bound(low, h.hints_0), ind_0.upper_bound(high, h.hints_0));
}
range<t_ind_0::iterator> equalRange_1(const t_tuple& t) const {
context h;
return equalRange_1(t, h);
}
range<t_ind_0::iterator> equalRange_7(const t_tuple& t, context& h) const {
auto pos = ind_0.find(t, h.hints_0);
auto fin = ind_0.end();
if (pos != fin) {fin = pos; ++fin;}
return make_range(pos, fin);
}
range<t_ind_0::iterator> equalRange_7(const t_tuple& t) const {
context h;
return equalRange_7(t, h);
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
o << prefix << "arity 3 direct b-tree index [0,1,2]: (hits/misses/total)\n";
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

class Sf_flights : public SouffleProgram {
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
	R"_(QA)_",
	R"_(VA)_",
	R"_(AU)_",
	R"_(CHI)_",
	R"_(JPN)_",
	R"_(DEN)_",
	R"_(ZA)_",
};// -- Table: flight
std::unique_ptr<t_btree_3__0_1_2__1__7> rel_1_flight = std::make_unique<t_btree_3__0_1_2__1__7>();
souffle::RelationWrapper<0,t_btree_3__0_1_2__1__7,Tuple<RamDomain,3>,3> wrapper_rel_1_flight;
// -- Table: QAflies
std::unique_ptr<t_btree_2__0_1__3> rel_2_QAflies = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<1,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_2_QAflies;
// -- Table: @delta_QAflies
std::unique_ptr<t_btree_2__0_1__1__3> rel_3_delta_QAflies = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: @new_QAflies
std::unique_ptr<t_btree_2__0_1__1__3> rel_4_new_QAflies = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: VAflies
std::unique_ptr<t_btree_2__0_1__3> rel_5_VAflies = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<2,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_5_VAflies;
// -- Table: @delta_VAflies
std::unique_ptr<t_btree_2__0_1__1__3> rel_6_delta_VAflies = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: @new_VAflies
std::unique_ptr<t_btree_2__0_1__1__3> rel_7_new_VAflies = std::make_unique<t_btree_2__0_1__1__3>();
// -- Table: QAonly
std::unique_ptr<t_btree_2__0_1__3> rel_8_QAonly = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<3,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_8_QAonly;
public:
Sf_flights() : 
wrapper_rel_1_flight(*rel_1_flight,symTable,"flight",std::array<const char *,3>{{"s:Node","s:Node","s:Node"}},std::array<const char *,3>{{"node1","node2","node3"}}),

wrapper_rel_2_QAflies(*rel_2_QAflies,symTable,"QAflies",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_5_VAflies(*rel_5_VAflies,symTable,"VAflies",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}),

wrapper_rel_8_QAonly(*rel_8_QAonly,symTable,"QAonly",std::array<const char *,2>{{"s:Node","s:Node"}},std::array<const char *,2>{{"node1","node2"}}){
addRelation("flight",&wrapper_rel_1_flight,false,false);
addRelation("QAflies",&wrapper_rel_2_QAflies,false,true);
addRelation("VAflies",&wrapper_rel_5_VAflies,false,true);
addRelation("QAonly",&wrapper_rel_8_QAonly,false,true);
}
~Sf_flights() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
SignalHandler::instance()->setMsg(R"_(flight("QA","AU","JPN").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [20:1-20:25])_");
rel_1_flight->insert(RamDomain(0),RamDomain(2),RamDomain(4));
SignalHandler::instance()->setMsg(R"_(flight("QA","JPN","DEN").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [21:1-21:26])_");
rel_1_flight->insert(RamDomain(0),RamDomain(4),RamDomain(5));
SignalHandler::instance()->setMsg(R"_(flight("QA","JPN","CHI").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [22:1-22:26])_");
rel_1_flight->insert(RamDomain(0),RamDomain(4),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(flight("QA","AU","CHI").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [23:1-23:25])_");
rel_1_flight->insert(RamDomain(0),RamDomain(2),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(flight("VA","AU","CHI").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [24:1-24:25])_");
rel_1_flight->insert(RamDomain(1),RamDomain(2),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(flight("VA","JPN","ZA").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [25:1-25:25])_");
rel_1_flight->insert(RamDomain(1),RamDomain(4),RamDomain(6));
SignalHandler::instance()->setMsg(R"_(flight("VA","JPN","DEN").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [26:1-26:26])_");
rel_1_flight->insert(RamDomain(1),RamDomain(4),RamDomain(5));
SignalHandler::instance()->setMsg(R"_(flight("VA","DEN","ZA").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [27:1-27:25])_");
rel_1_flight->insert(RamDomain(1),RamDomain(5),RamDomain(6));
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
SignalHandler::instance()->setMsg(R"_(QAflies(X,Y) :- 
   flight("QA",X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [11:1-11:34])_");
if(!(rel_1_flight->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_QAflies_op_ctxt,rel_2_QAflies->createContext());
CREATE_OP_CONTEXT(rel_1_flight_op_ctxt,rel_1_flight->createContext());
const Tuple<RamDomain,3> key({{RamDomain(0),0,0}});
auto range = rel_1_flight->equalRange_1(key,READ_OP_CONTEXT(rel_1_flight_op_ctxt));
for(const auto& env0 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env0[2])}});
rel_2_QAflies->insert(tuple,READ_OP_CONTEXT(rel_2_QAflies_op_ctxt));
}
}
();}
rel_3_delta_QAflies->insertAll(*rel_2_QAflies);
iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(QAflies(X,Y) :- 
   flight("QA",X,Z),
   QAflies(Z,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [12:1-12:48])_");
if(!(rel_3_delta_QAflies->empty()) && !(rel_1_flight->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_delta_QAflies_op_ctxt,rel_3_delta_QAflies->createContext());
CREATE_OP_CONTEXT(rel_2_QAflies_op_ctxt,rel_2_QAflies->createContext());
CREATE_OP_CONTEXT(rel_1_flight_op_ctxt,rel_1_flight->createContext());
CREATE_OP_CONTEXT(rel_4_new_QAflies_op_ctxt,rel_4_new_QAflies->createContext());
const Tuple<RamDomain,3> key({{RamDomain(0),0,0}});
auto range = rel_1_flight->equalRange_1(key,READ_OP_CONTEXT(rel_1_flight_op_ctxt));
for(const auto& env0 : range) {
const Tuple<RamDomain,2> key({{env0[2],0}});
auto range = rel_3_delta_QAflies->equalRange_1(key,READ_OP_CONTEXT(rel_3_delta_QAflies_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_2_QAflies->contains(Tuple<RamDomain,2>({{env0[1],env1[1]}}),READ_OP_CONTEXT(rel_2_QAflies_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env1[1])}});
rel_4_new_QAflies->insert(tuple,READ_OP_CONTEXT(rel_4_new_QAflies_op_ctxt));
}
}
}
}
();}
if(rel_4_new_QAflies->empty()) break;
rel_2_QAflies->insertAll(*rel_4_new_QAflies);
std::swap(rel_3_delta_QAflies, rel_4_new_QAflies);
rel_4_new_QAflies->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled()) rel_3_delta_QAflies->purge();
if (!isHintsProfilingEnabled()) rel_4_new_QAflies->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./QAflies.csv"},{"name","QAflies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_QAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}();
/* END STRATUM 1 */
/* BEGIN STRATUM 2 */
[&]() {
SignalHandler::instance()->setMsg(R"_(VAflies(X,Y) :- 
   flight("VA",X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [14:1-14:34])_");
if(!(rel_1_flight->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_1_flight_op_ctxt,rel_1_flight->createContext());
CREATE_OP_CONTEXT(rel_5_VAflies_op_ctxt,rel_5_VAflies->createContext());
const Tuple<RamDomain,3> key({{RamDomain(1),0,0}});
auto range = rel_1_flight->equalRange_1(key,READ_OP_CONTEXT(rel_1_flight_op_ctxt));
for(const auto& env0 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env0[2])}});
rel_5_VAflies->insert(tuple,READ_OP_CONTEXT(rel_5_VAflies_op_ctxt));
}
}
();}
rel_6_delta_VAflies->insertAll(*rel_5_VAflies);
iter = 0;
for(;;) {
SignalHandler::instance()->setMsg(R"_(VAflies(X,Y) :- 
   flight("VA",X,Z),
   VAflies(Z,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [15:1-15:48])_");
if(!(rel_6_delta_VAflies->empty()) && !(rel_1_flight->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_7_new_VAflies_op_ctxt,rel_7_new_VAflies->createContext());
CREATE_OP_CONTEXT(rel_1_flight_op_ctxt,rel_1_flight->createContext());
CREATE_OP_CONTEXT(rel_6_delta_VAflies_op_ctxt,rel_6_delta_VAflies->createContext());
CREATE_OP_CONTEXT(rel_5_VAflies_op_ctxt,rel_5_VAflies->createContext());
const Tuple<RamDomain,3> key({{RamDomain(1),0,0}});
auto range = rel_1_flight->equalRange_1(key,READ_OP_CONTEXT(rel_1_flight_op_ctxt));
for(const auto& env0 : range) {
const Tuple<RamDomain,2> key({{env0[2],0}});
auto range = rel_6_delta_VAflies->equalRange_1(key,READ_OP_CONTEXT(rel_6_delta_VAflies_op_ctxt));
for(const auto& env1 : range) {
if( !(rel_5_VAflies->contains(Tuple<RamDomain,2>({{env0[1],env1[1]}}),READ_OP_CONTEXT(rel_5_VAflies_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env1[1])}});
rel_7_new_VAflies->insert(tuple,READ_OP_CONTEXT(rel_7_new_VAflies_op_ctxt));
}
}
}
}
();}
if(rel_7_new_VAflies->empty()) break;
rel_5_VAflies->insertAll(*rel_7_new_VAflies);
std::swap(rel_6_delta_VAflies, rel_7_new_VAflies);
rel_7_new_VAflies->purge();
iter++;
}
iter = 0;
if (!isHintsProfilingEnabled()) rel_6_delta_VAflies->purge();
if (!isHintsProfilingEnabled()) rel_7_new_VAflies->purge();
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./VAflies.csv"},{"name","VAflies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_VAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_1_flight->purge();
}();
/* END STRATUM 2 */
/* BEGIN STRATUM 3 */
[&]() {
SignalHandler::instance()->setMsg(R"_(QAonly(X,Y) :- 
   QAflies(X,Y),
   !VAflies(X,Y).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/java/flights/flights.dl [17:1-17:45])_");
if(!(rel_2_QAflies->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_2_QAflies_op_ctxt,rel_2_QAflies->createContext());
CREATE_OP_CONTEXT(rel_8_QAonly_op_ctxt,rel_8_QAonly->createContext());
CREATE_OP_CONTEXT(rel_5_VAflies_op_ctxt,rel_5_VAflies->createContext());
for(const auto& env0 : *rel_2_QAflies) {
if( !(rel_5_VAflies->contains(Tuple<RamDomain,2>({{env0[0],env0[1]}}),READ_OP_CONTEXT(rel_5_VAflies_op_ctxt)))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[0]),static_cast<RamDomain>(env0[1])}});
rel_8_QAonly->insert(tuple,READ_OP_CONTEXT(rel_8_QAonly_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./QAonly.csv"},{"name","QAonly"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_QAonly);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_5_VAflies->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_2_QAflies->purge();
}();
/* END STRATUM 3 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_flight:\n";
rel_1_flight->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_QAflies:\n";
rel_2_QAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_delta_QAflies:\n";
rel_3_delta_QAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_new_QAflies:\n";
rel_4_new_QAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_5_VAflies:\n";
rel_5_VAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_6_delta_VAflies:\n";
rel_6_delta_VAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_7_new_VAflies:\n";
rel_7_new_VAflies->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_8_QAonly:\n";
rel_8_QAonly->printHintStatistics(std::cout,"  ");
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./QAflies.csv"},{"name","QAflies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_QAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./VAflies.csv"},{"name","VAflies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_VAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","node1\tnode2"},{"filename","./QAonly.csv"},{"name","QAonly"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_QAonly);
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
ioDirectives.setRelationName("rel_2_QAflies");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_2_QAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_5_VAflies");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_VAflies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_8_QAonly");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_8_QAonly);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
};
SouffleProgram *newInstance_flights(){return new Sf_flights;}
SymbolTable *getST_flights(SouffleProgram *p){return &reinterpret_cast<Sf_flights*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_flights: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_flights();
};
public:
factory_Sf_flights() : ProgramFactory("flights"){}
};
static factory_Sf_flights __factory_Sf_flights_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(swig/java/flights/flights.dl)",
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
souffle::Sf_flights obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
