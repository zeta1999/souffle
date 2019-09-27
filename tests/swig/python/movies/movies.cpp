
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
using namespace ram;
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
range<iterator> equalRange_0(const t_tuple& t) const {
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

class Sf_movies : public SouffleProgram {
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
	R"_(Movie a)_",
	R"_(2.3/10)_",
	R"_(Movie b)_",
	R"_(9.3/10)_",
	R"_(Movie c)_",
	R"_(8.6/10)_",
	R"_(John Smith)_",
	R"_(Joe Bloggs)_",
	R"_(Francesca Allsworth)_",
	R"_(Julia Thebest)_",
};// -- Table: Actors
std::unique_ptr<t_btree_2__0_1__1__3> rel_1_Actors = std::make_unique<t_btree_2__0_1__1__3>();
souffle::RelationWrapper<0,t_btree_2__0_1__1__3,Tuple<RamDomain,2>,2> wrapper_rel_1_Actors;
// -- Table: Movie
std::unique_ptr<t_btree_3__0_1_2__1__7> rel_2_Movie = std::make_unique<t_btree_3__0_1_2__1__7>();
souffle::RelationWrapper<1,t_btree_3__0_1_2__1__7,Tuple<RamDomain,3>,3> wrapper_rel_2_Movie;
// -- Table: MovieActors
std::unique_ptr<t_btree_2__0_1__3> rel_3_MovieActors = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<2,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_3_MovieActors;
// -- Table: MovieActorsText
std::unique_ptr<t_btree_2__0_1__3> rel_4_MovieActorsText = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<3,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_4_MovieActorsText;
// -- Table: ActorsRatings
std::unique_ptr<t_btree_2__0_1__3> rel_5_ActorsRatings = std::make_unique<t_btree_2__0_1__3>();
souffle::RelationWrapper<4,t_btree_2__0_1__3,Tuple<RamDomain,2>,2> wrapper_rel_5_ActorsRatings;
// -- Table: JoeBloggsMovies
std::unique_ptr<t_btree_1__0__1> rel_6_JoeBloggsMovies = std::make_unique<t_btree_1__0__1>();
souffle::RelationWrapper<5,t_btree_1__0__1,Tuple<RamDomain,1>,1> wrapper_rel_6_JoeBloggsMovies;
public:
Sf_movies() : 
wrapper_rel_1_Actors(*rel_1_Actors,symTable,"Actors",std::array<const char *,2>{{"i:number","s:Text"}},std::array<const char *,2>{{"actorid","name"}}),

wrapper_rel_2_Movie(*rel_2_Movie,symTable,"Movie",std::array<const char *,3>{{"i:number","s:Text","s:Text"}},std::array<const char *,3>{{"id","title","rating"}}),

wrapper_rel_3_MovieActors(*rel_3_MovieActors,symTable,"MovieActors",std::array<const char *,2>{{"i:number","i:number"}},std::array<const char *,2>{{"movieid","actorsid"}}),

wrapper_rel_4_MovieActorsText(*rel_4_MovieActorsText,symTable,"MovieActorsText",std::array<const char *,2>{{"s:Text","s:Text"}},std::array<const char *,2>{{"title","actorname"}}),

wrapper_rel_5_ActorsRatings(*rel_5_ActorsRatings,symTable,"ActorsRatings",std::array<const char *,2>{{"s:Text","s:Text"}},std::array<const char *,2>{{"name","rating"}}),

wrapper_rel_6_JoeBloggsMovies(*rel_6_JoeBloggsMovies,symTable,"JoeBloggsMovies",std::array<const char *,1>{{"s:Text"}},std::array<const char *,1>{{"title"}}){
addRelation("Actors",&wrapper_rel_1_Actors,false,false);
addRelation("Movie",&wrapper_rel_2_Movie,false,false);
addRelation("MovieActors",&wrapper_rel_3_MovieActors,false,false);
addRelation("MovieActorsText",&wrapper_rel_4_MovieActorsText,false,true);
addRelation("ActorsRatings",&wrapper_rel_5_ActorsRatings,false,true);
addRelation("JoeBloggsMovies",&wrapper_rel_6_JoeBloggsMovies,false,true);
}
~Sf_movies() {
}
private:
void runFunction(std::string inputDirectory = ".", std::string outputDirectory = ".", size_t stratumIndex = (size_t) -1, bool performIO = false) {
SignalHandler::instance()->set();
std::atomic<size_t> iter(0);

// -- query evaluation --
/* BEGIN STRATUM 0 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Actors(1,"John Smith").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [27:1-27:24])_");
rel_1_Actors->insert(RamDomain(1),RamDomain(6));
SignalHandler::instance()->setMsg(R"_(Actors(2,"Joe Bloggs").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [28:1-28:24])_");
rel_1_Actors->insert(RamDomain(2),RamDomain(7));
SignalHandler::instance()->setMsg(R"_(Actors(3,"Francesca Allsworth").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [29:1-29:33])_");
rel_1_Actors->insert(RamDomain(3),RamDomain(8));
SignalHandler::instance()->setMsg(R"_(Actors(4,"Julia Thebest").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [30:1-30:27])_");
rel_1_Actors->insert(RamDomain(4),RamDomain(9));
}();
/* END STRATUM 0 */
/* BEGIN STRATUM 1 */
[&]() {
SignalHandler::instance()->setMsg(R"_(Movie(1,"Movie a","2.3/10").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [14:1-14:29])_");
rel_2_Movie->insert(RamDomain(1),RamDomain(0),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(Movie(2,"Movie b","9.3/10").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [15:1-15:29])_");
rel_2_Movie->insert(RamDomain(2),RamDomain(2),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(Movie(3,"Movie c","8.6/10").
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [16:1-16:29])_");
rel_2_Movie->insert(RamDomain(3),RamDomain(4),RamDomain(5));
}();
/* END STRATUM 1 */
/* BEGIN STRATUM 2 */
[&]() {
SignalHandler::instance()->setMsg(R"_(MovieActors(1,1).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [18:1-18:18])_");
rel_3_MovieActors->insert(RamDomain(1),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(MovieActors(1,2).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [19:1-19:18])_");
rel_3_MovieActors->insert(RamDomain(1),RamDomain(2));
SignalHandler::instance()->setMsg(R"_(MovieActors(1,3).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [20:1-20:18])_");
rel_3_MovieActors->insert(RamDomain(1),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(MovieActors(2,2).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [21:1-21:18])_");
rel_3_MovieActors->insert(RamDomain(2),RamDomain(2));
SignalHandler::instance()->setMsg(R"_(MovieActors(2,3).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [22:1-22:18])_");
rel_3_MovieActors->insert(RamDomain(2),RamDomain(3));
SignalHandler::instance()->setMsg(R"_(MovieActors(2,4).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [23:1-23:18])_");
rel_3_MovieActors->insert(RamDomain(2),RamDomain(4));
SignalHandler::instance()->setMsg(R"_(MovieActors(3,1).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [24:1-24:18])_");
rel_3_MovieActors->insert(RamDomain(3),RamDomain(1));
SignalHandler::instance()->setMsg(R"_(MovieActors(3,4).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [25:1-25:18])_");
rel_3_MovieActors->insert(RamDomain(3),RamDomain(4));
}();
/* END STRATUM 2 */
/* BEGIN STRATUM 3 */
[&]() {
SignalHandler::instance()->setMsg(R"_(MovieActorsText(c,d) :- 
   MovieActors(a,b),
   Movie(a,c,_),
   Actors(b,d).
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [37:1-37:69])_");
if(!(rel_1_Actors->empty()) && !(rel_2_Movie->empty()) && !(rel_3_MovieActors->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_3_MovieActors_op_ctxt,rel_3_MovieActors->createContext());
CREATE_OP_CONTEXT(rel_4_MovieActorsText_op_ctxt,rel_4_MovieActorsText->createContext());
CREATE_OP_CONTEXT(rel_2_Movie_op_ctxt,rel_2_Movie->createContext());
CREATE_OP_CONTEXT(rel_1_Actors_op_ctxt,rel_1_Actors->createContext());
for(const auto& env0 : *rel_3_MovieActors) {
const Tuple<RamDomain,3> key({{env0[0],0,0}});
auto range = rel_2_Movie->equalRange_1(key,READ_OP_CONTEXT(rel_2_Movie_op_ctxt));
for(const auto& env1 : range) {
const Tuple<RamDomain,2> key({{env0[1],0}});
auto range = rel_1_Actors->equalRange_1(key,READ_OP_CONTEXT(rel_1_Actors_op_ctxt));
for(const auto& env2 : range) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env1[1]),static_cast<RamDomain>(env2[1])}});
rel_4_MovieActorsText->insert(tuple,READ_OP_CONTEXT(rel_4_MovieActorsText_op_ctxt));
}
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","title\tactorname"},{"filename","./MovieActorsText.csv"},{"name","MovieActorsText"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_MovieActorsText);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_1_Actors->purge();
if (!isHintsProfilingEnabled()&& performIO) rel_3_MovieActors->purge();
}();
/* END STRATUM 3 */
/* BEGIN STRATUM 4 */
[&]() {
SignalHandler::instance()->setMsg(R"_(ActorsRatings(a,rating) :- 
   MovieActorsText(movie,a),
   Movie(_,movie_b,rating),
   movie match movie_b.
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [47:1-47:101])_");
if(!(rel_2_Movie->empty()) && !(rel_4_MovieActorsText->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_ActorsRatings_op_ctxt,rel_5_ActorsRatings->createContext());
CREATE_OP_CONTEXT(rel_4_MovieActorsText_op_ctxt,rel_4_MovieActorsText->createContext());
CREATE_OP_CONTEXT(rel_2_Movie_op_ctxt,rel_2_Movie->createContext());
for(const auto& env0 : *rel_4_MovieActorsText) {
for(const auto& env1 : *rel_2_Movie) {
if( regex_wrapper(symTable.resolve(env0[0]),symTable.resolve(env1[1]))) {
Tuple<RamDomain,2> tuple({{static_cast<RamDomain>(env0[1]),static_cast<RamDomain>(env1[2])}});
rel_5_ActorsRatings->insert(tuple,READ_OP_CONTEXT(rel_5_ActorsRatings_op_ctxt));
}
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","name\trating"},{"filename","./ActorsRatings.csv"},{"name","ActorsRatings"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_ActorsRatings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_2_Movie->purge();
}();
/* END STRATUM 4 */
/* BEGIN STRATUM 5 */
[&]() {
SignalHandler::instance()->setMsg(R"_(JoeBloggsMovies(a) :- 
   MovieActorsText(a,b),
   "Joe Bloggs" match b.
in file /mnt/c/Users/lnimn/Videos/stufflisa/school/uni/YR3SEM2/COMP3888/CAPSTONE/project19-group5/souffle/tests/swig/python/movies/movies.dl [42:1-42:69])_");
if(!(rel_4_MovieActorsText->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_MovieActorsText_op_ctxt,rel_4_MovieActorsText->createContext());
CREATE_OP_CONTEXT(rel_6_JoeBloggsMovies_op_ctxt,rel_6_JoeBloggsMovies->createContext());
for(const auto& env0 : *rel_4_MovieActorsText) {
if( regex_wrapper(symTable.resolve(RamDomain(7)),symTable.resolve(env0[1]))) {
Tuple<RamDomain,1> tuple({{static_cast<RamDomain>(env0[0])}});
rel_6_JoeBloggsMovies->insert(tuple,READ_OP_CONTEXT(rel_6_JoeBloggsMovies_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","title"},{"filename","./JoeBloggsMovies.csv"},{"name","JoeBloggsMovies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1}), symTable, ioDirectives, false)->writeAll(*rel_6_JoeBloggsMovies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (!isHintsProfilingEnabled()&& performIO) rel_4_MovieActorsText->purge();
}();
/* END STRATUM 5 */

// -- relation hint statistics --
if(isHintsProfilingEnabled()) {
std::cout << " -- Operation Hint Statistics --\n";
std::cout << "Relation rel_1_Actors:\n";
rel_1_Actors->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_2_Movie:\n";
rel_2_Movie->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_3_MovieActors:\n";
rel_3_MovieActors->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_4_MovieActorsText:\n";
rel_4_MovieActorsText->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_5_ActorsRatings:\n";
rel_5_ActorsRatings->printHintStatistics(std::cout,"  ");
std::cout << "\n";
std::cout << "Relation rel_6_JoeBloggsMovies:\n";
rel_6_JoeBloggsMovies->printHintStatistics(std::cout,"  ");
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
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","title\tactorname"},{"filename","./MovieActorsText.csv"},{"name","MovieActorsText"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_MovieActorsText);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","name\trating"},{"filename","./ActorsRatings.csv"},{"name","ActorsRatings"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_ActorsRatings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","title"},{"filename","./JoeBloggsMovies.csv"},{"name","JoeBloggsMovies"}});
if (!outputDirectory.empty() && directiveMap["IO"] == "file" && directiveMap["filename"].front() != '/') {directiveMap["filename"] = outputDirectory + "/" + directiveMap["filename"];}
IODirectives ioDirectives(directiveMap);
IOSystem::getInstance().getWriter(std::vector<bool>({1}), symTable, ioDirectives, false)->writeAll(*rel_6_JoeBloggsMovies);
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
ioDirectives.setRelationName("rel_4_MovieActorsText");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_4_MovieActorsText);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_5_ActorsRatings");
IOSystem::getInstance().getWriter(std::vector<bool>({1,1}), symTable, ioDirectives, false)->writeAll(*rel_5_ActorsRatings);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {IODirectives ioDirectives;
ioDirectives.setIOType("stdout");
ioDirectives.setRelationName("rel_6_JoeBloggsMovies");
IOSystem::getInstance().getWriter(std::vector<bool>({1}), symTable, ioDirectives, false)->writeAll(*rel_6_JoeBloggsMovies);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
};
SouffleProgram *newInstance_movies(){return new Sf_movies;}
SymbolTable *getST_movies(SouffleProgram *p){return &reinterpret_cast<Sf_movies*>(p)->symTable;}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_movies: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_movies();
};
public:
factory_Sf_movies() : ProgramFactory("movies"){}
};
static factory_Sf_movies __factory_Sf_movies_instance;
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(swig/python/movies/movies.dl)",
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
souffle::Sf_movies obj;
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir(), opt.getStratumIndex());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
