import net.wasamon.javarock.model.VHDLGenericParameter;

import net.wasamon.javarock.model.VHDLPort;
import net.wasamon.javarock.model.VHDLComponentIface;
import net.wasamon.javarock.tools.types.VHDLTypeBuilder;
import net.wasamon.javarock.libraries.VHDLSimpleLibrary;

import net.wasamon.javarock.rt.*;

public class ButtonIF extends VHDLSimpleLibrary implements VHDLComponentIface{
    
    public boolean pSW0;
    public boolean pSW1;
    public boolean pSW2;
    public boolean pSW3;

    public ButtonIF(String... args){
        super("button", args);
        ports.add(new VHDLPort("pSW0", VHDLTypeBuilder.getStdLogic(), VHDLPort.Dir.IN, VHDLPort.Kind.EXTERNAL));
        ports.add(new VHDLPort("pSW1", VHDLTypeBuilder.getStdLogic(), VHDLPort.Dir.IN, VHDLPort.Kind.EXTERNAL));
        ports.add(new VHDLPort("pSW2", VHDLTypeBuilder.getStdLogic(), VHDLPort.Dir.IN, VHDLPort.Kind.EXTERNAL));
        ports.add(new VHDLPort("pSW3", VHDLTypeBuilder.getStdLogic(), VHDLPort.Dir.IN, VHDLPort.Kind.EXTERNAL));
    }
}