#include <signal_path/signal_path.h>

namespace sigpath {
    IQFrontEnd iqFrontEnd;
    VFOManager vfoManager;
    SourceManager sourceManager;
    SinkManager sinkManager;
    Transmitter *transmitter;
    Event<bool> txState;

};