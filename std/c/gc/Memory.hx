package c.gc;

@:coreType
abstract Memory<T> to T {
	
	public function new() this = null;
	public inline function get():T return this;
}

@:keep
@:coreType
abstract XMemory<T> to T {
    
    public function new() this = null;
    
    
}