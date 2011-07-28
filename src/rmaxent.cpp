/*
 *  FILE: rmaxent.cpp
 *  AUTHOR: Timothy Jurka
 *  DATE: July 2011
 *  DESCRIPTION: C++ to R interface for the Maximum Entropy library written by Yoshimasa Tsuruoka.
 */

#include <Rcpp.h>
#include <R.h>
#include <Rmath.h>
#include <string>
#include <sstream>
#include <iomanip>
#include <list>
#include <cmath>
#include <cstdio>
#include "maxent.h"

using namespace std;
using namespace Rcpp;

// Global variable, model
ME_Model model;

// New model
void new_model() {
	model = *(new ME_Model());
}

// Add sample data
void add_samples(vector<string> samples, vector<string> features, SEXP matrix) {
	new_model();
	NumericMatrix tdm(matrix);
	
	for (int i=0; i < tdm.nrow(); i++) { // for each document
		//Rprintf("Document %d\n",i); // debug output
		ME_Sample newSample(samples[i]); // create new sample for code
		for (int j=0; j < tdm.ncol(); j++) { // for each feature
			//Rprintf("Feature %s\n",features[j].c_str());
			if (tdm(i,j) != 0) newSample.add_feature(features[j],tdm(i,j));
		}
		model.add_training_sample(newSample);
	}
}

void add_samples_sparse(int nrows, int ncols, vector<string> samples, vector<int> ia, vector<string> ja, vector<double> ra) {
	new_model();
	for (int i=0; i < nrows; i++) { // for each document
		//Rprintf("Document %d\n",i); // debug output
		ME_Sample newSample(samples[i]); // create new sample for code
		for (int j=ia[i]-1; j < ia[i+1]-1; j++) { // for each feature
			//Rprintf("Feature %s\n",features[j].c_str());
			newSample.add_feature(ja[j],ra[j]);
		}
		model.add_training_sample(newSample);
	}
}

// Classify
RcppExport SEXP classify_samples(vector<string> features, SEXP matrix, string model_data) {
	new_model();
	model.load_from_string(model_data);
	NumericMatrix tdm(matrix);
	NumericMatrix probability_matrix(tdm.nrow(),model.num_classes());

	vector<string> results;
	vector<string> probability_names;

	for (int i=0; i < tdm.nrow(); i++) { // for each document
		//Rprintf("Document %d\n",i); // debug output
		ME_Sample newSample; // create new sample for code
		for (int j=0; j < tdm.ncol(); j++) { // for each feature
			if (tdm(i,j) != 0) newSample.add_feature(features[j],tdm(i,j));
		}
		
		vector<double> prob = model.classify(newSample);
		for (int k=0; k < model.num_classes(); k++) {
			probability_matrix(i,k) = prob[k];
		}
		
		results.push_back(newSample.label);
		//Rprintf("Probability %d\n",newSample.probability);
	}
	
	for (int k=0; k < model.num_classes(); k++) {
		probability_names.push_back(model.get_class_label(k));
	}
	
	List rs = List::create(results,probability_matrix,probability_names);
	
	return rs;
}

RcppExport SEXP classify_samples_sparse(int nrows, int ncols, vector<int> ia, vector<string> ja, vector<double> ra, string model_data) {
	new_model();
	model.load_from_string(model_data);
	vector<string> results;
	vector<string> probability_names;
	
	NumericMatrix probability_matrix(nrows,model.num_classes());
	
	for (int i=0; i < nrows; i++) { // for each document
		//Rprintf("Document %d\n",i); // debug output
		ME_Sample newSample; // create new sample for code
		for (int j=ia[i]-1; j < ia[i+1]-1; j++) { // for each feature
			newSample.add_feature(ja[j],ra[j]);
		}
		
		vector<double> prob = model.classify(newSample);
		for (int k=0; k < model.num_classes(); k++) {
			probability_matrix(i,k) = prob[k];
		}
		
		results.push_back(newSample.label);
		//Rprintf("Probability %d\n",log(newSample.probability)/10);
	}
	
	for (int k=0; k < model.num_classes(); k++) {
		probability_names.push_back(model.get_class_label(k));
	}
	
	List rs = List::create(results,probability_matrix,probability_names);
	
	return rs;
}

// Export weights
vector< vector<string> > export_weights() {
	list< pair< pair<string, string>, double > > fl;
	model.get_features(fl);
	
	vector<string> value1;
	vector<string> value2;
	vector<string> value3;
	for (list< pair< pair<string, string>, double> >::const_iterator i = fl.begin(); i != fl.end(); i++) {
		stringstream write_weights1;
		write_weights1 << setprecision(3) << setw(10) << i->second;
		string weights1 = write_weights1.str();
		value1.push_back(weights1);
		
		stringstream write_weights2;
		write_weights2 << left << setw(10) << i->first.first.c_str();
		string weights2 = write_weights2.str();
		value2.push_back(weights2);
		
		stringstream write_weights3;
		write_weights3 << i->first.second.c_str();
		string weights3 = write_weights3.str();
		value3.push_back(weights3);
	}
	
	vector< vector<string> > results;
	results.push_back(value1);
	results.push_back(value2);
	results.push_back(value3);
	
	return results;
}

// Print weights
void print_weights() {
	list< pair< pair<string, string>, double > > fl;
	model.get_features(fl);
	for (list< pair< pair<string, string>, double> >::const_iterator i = fl.begin(); i != fl.end(); i++) {
		Rprintf("%10.3f  %-10s %s\n", i->second, i->first.first.c_str(), i->first.second.c_str());
	}
}

// Train model
RcppExport SEXP train_model(int cutoff=0, double sigma=0, double widthfactor=0, int heldout=0) {
	Rprintf("Training the new model...\n");
	if (heldout > 0) model.set_heldout(heldout);
	model.train(cutoff,sigma,widthfactor);
	
	string model_data = model.save_to_string();
	vector< vector<string> > weights = export_weights();
	List rs = List::create(model_data,weights[0],weights[1],weights[2]);
	
	return rs;
}

RCPP_MODULE(maximumentropy) {
	using namespace Rcpp;
	function("add_samples", &add_samples);
	function("add_samples_sparse", &add_samples_sparse);
	function("classify_samples", &classify_samples);
	function("classify_samples_sparse", &classify_samples_sparse);
	function("new_model", &new_model);
	function("train_model", &train_model);
	function("export_weights", &export_weights);
	function("print_weights", &print_weights);
}
