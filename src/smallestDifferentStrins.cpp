
#include <algorithm>
#include <vector>
#include <Rcpp.h>

//' @title Remove the substring which is shared by the beginning and end of all strings in a CharacterVector.
//'
//' @param cStrings The CharacterVector.
//' @param verbose Print verbose output?
//' @return CharacterVector with common beginning and ending removed.
//'
// [[Rcpp::export]]
Rcpp::CharacterVector smallestDifferentStrings(Rcpp::CharacterVector cStrings, bool verbose = true)
{
    // get min string length
    std::vector<std::string> strings;
    size_t min_len = std::string::npos;
    for(size_t i = 0; i < cStrings.size(); i++) {
        strings.push_back(std::string(cStrings[i]));
        min_len = std::min(min_len, strings.back().size());
    }

    // debugging variables
    std::string commonStart, commonEnd = "";

    // remove common start
    bool done = false;
    for(size_t i = 0; i < min_len; i++){
        char c = strings[0][i];
        for(size_t j = 1; j < strings.size(); j++){
            if(c != strings[j][i]){
                done = true;
                break;
            }
        }
        if(done) break;
        commonStart += c;
    }
    for(size_t i = 0; i < strings.size(); i++){
        strings[i].erase(0, commonStart.size());
    }

    // remove common end
    done = false;
    for(size_t i = 0; i < min_len; i++){
        char c = *(strings[0].rbegin() + i);
        for(size_t j = 1; j < strings.size(); j++){
            if(c != *(strings[j].rbegin() + i)){
                done = true;
                break;
            }
        }
        if(done) break;
        commonEnd += c;
    }
    std::reverse(commonEnd.begin(), commonEnd.end());
    for(size_t i = 0; i < strings.size(); i++){
        strings[i].erase(strings[i].size() - commonEnd.size());
    }

    if(verbose)
        Rcpp::Rcout << "commonStart = " << commonStart << '\n'
            << "commonEnd = " << commonEnd << '\n';

    Rcpp::CharacterVector ret;
    for(auto s: strings) ret.push_back(s.c_str());

    return ret;
}

