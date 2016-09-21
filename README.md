"# computational-linguistics" 

This an implementation of a HMM tagger for the common part-of-speech tagging task in NLP.

It achieves great accuracy on the Penn-TreeBank corpora.

It has been implemented in the F# programming language.

To run the trainer just run the WholeTestSet.fsx or KFold.fsx, respectively training the model on the whole corpora and the latter
applying different 90-10 splits on the data for training and validation.
