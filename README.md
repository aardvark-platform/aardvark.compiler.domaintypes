# aardvark.compiler.domaintypes

Generates incremental datastructures (from Aardvark.Base.Incremental) from purely functional input data types. 
Generated types can be used to compute diffs on immutable input values in order to feed changes incrementally into the mutable variants. 
This is heavily used by aardvark.media. 
Additionally to a standalone source to source compiler this functionality is exposed via a msbuild build step.
