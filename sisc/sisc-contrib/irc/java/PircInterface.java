import org.jibble.pircbot.PircBot;
import sisc.interpreter.*;
import sisc.data.*;
import sisc.modules.s2j.*;
import sisc.util.Util;

public class PircInterface extends PircBot {

    Procedure callHandler;
    Interpreter interp;

    public PircInterface(String name) {
        this.setName(name);
        this.setLogin("siscbot");
        interp=Context.enter();
        try {
            callHandler=(Procedure)interp.eval(Symbol.get("call-handler"));
        } catch (SchemeException e) {
            e.printStackTrace();
        }
    }

    void call(Symbol functionName, JavaObject[] args) {
        System.err.println("Call: "+functionName);
        try {
            Value[] v=new Value[args.length+1];
            v[0]=functionName;
            System.arraycopy(args,0,v,1,args.length);
            interp.eval(callHandler, v);
        } catch (SchemeException e) {
            e.printStackTrace();
        }
    }

    protected void onConnect() {
	call(Symbol.get("onConnect"),
             new JavaObject[0]);
    }

    protected void onDisconnect() {
        call(Symbol.get("onDisconnect"),
             new JavaObject[0]);
    }

    protected void onServerResponse(int v0,java.lang.String v1) {
        call(Symbol.get("onServerResponse"),
             new JavaObject[] {
                 new JavaObject(new Integer(v0)), new JavaObject(v1)});
    }

    protected void onServerPing(java.lang.String v0) {
	call(Symbol.get("onServerPing"),
             new JavaObject[] {
                 new JavaObject(v0)});
    }

    protected void onMessage(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onMessage"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onPrivateMessage(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onPrivateMessage"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onAction(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onAction"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onJoin(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onJoin"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onPart(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onPart"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onNickChange(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onNickChange"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onKick(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4,java.lang.String v5) {
	call(Symbol.get("onKick"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4), new JavaObject(v5)});
    }

    protected void onQuit(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onQuit"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onMode(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onMode"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onOp(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onOp"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onDeop(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onDeop"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onVoice(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onVoice"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onDeVoice(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onDeVoice"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onSetChannelKey(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onSetChannelKey"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onRemoveChannelKey(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onRemoveChannelKey"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onSetChannelLimit(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,int v4) {
	call(Symbol.get("onSetChannelLimit"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(new Integer(v4))});
    }

    protected void onRemoveChannelLimit(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemoveChannelLimit"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetChannelBan(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onSetChannelBan"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onRemoveChannelBan(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onRemoveChannelBan"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onSetTopicProtection(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetTopicProtection"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onRemoveTopicProtection(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemoveTopicProtection"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetNoExternalMessages(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetNoExternalMessages"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onRemoveNoExternalMessages(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemoveNoExternalMessages"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetInviteOnly(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetInviteOnly"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }
    
    protected void onRemoveInviteOnly(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
        call(Symbol.get("onRemoveInviteOnly"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetModerated(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetModerated"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onRemoveModerated(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemoveModerated"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetPrivate(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetPrivate"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onRemovePrivate(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemovePrivate"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onSetSecret(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onSetSecret"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onRemoveSecret(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onRemoveSecret"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onInvite(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onInvite"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onDccSendRequest(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,long v4,int v5,int v6) {
	call(Symbol.get("onDccSendRequest"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(new Long(v4)), new JavaObject(new Integer(v5)), new JavaObject(new Integer(v6))});
    }

    protected void onDccChatRequest(java.lang.String v0,java.lang.String v1,java.lang.String v2,long v3,int v4) {
	call(Symbol.get("onDccChatRequest"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(new Long(v3)), new JavaObject(new Integer(v4))});
    }

    protected void onVersion(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onVersion"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }

    protected void onPing(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onPing"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onTime(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
        call(Symbol.get("onTime"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }
    
    protected void onFinger(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3) {
	call(Symbol.get("onFinger"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3)});
    }
    
    protected void onNotice(java.lang.String v0,java.lang.String v1,java.lang.String v2,java.lang.String v3,java.lang.String v4) {
	call(Symbol.get("onNotice"),
             new JavaObject[] {
                 new JavaObject(v0), new JavaObject(v1), new JavaObject(v2), new JavaObject(v3), new JavaObject(v4)});
    }

    protected void onUnknown(java.lang.String v0) {
	call(Symbol.get("onUnknown"),
             new JavaObject[] {
                 new JavaObject(v0)});
    }
}
