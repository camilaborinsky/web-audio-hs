data AudioParam a = A a
                    | K a

data SourceNode = Oscillator
                    | Buffer
                    | MediaStream

data EffectNode = BiquadFilter
                    | Convolver
                    | Delay
                    | DynamicsCompressor
                    | Gain
                    | WaveShaper

data DestinationNode = Destination
                        | MediaStreamDestination


data AnalysisNode = Analyser

data ChannelNode = ChannelSplitter 
                    | ChannelMerger

data SpatializationNode = StereoPanner
                    | Panner
                    

data AudioNode = Source SourceNode
                | Dest DestinationNode
                | Effect EffectNode
                | Analysis AnalysisNode
                | Spatialization SpatializationNode
                | Channel ChannelNode

data AudioGraph = AudioNode