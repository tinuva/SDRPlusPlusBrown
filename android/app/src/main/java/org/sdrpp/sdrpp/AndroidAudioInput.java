package org.sdrpp.sdrpp;

import android.util.Log;

public class AndroidAudioInput {
    public static native void addAudioSamples(float[] arr);
    public static void startAudioCapture() {

    }
    public static void stopAudioCapture() {

    }

    public static void logcat(byte[] data) {
        Log.println(Log.INFO, "audio", new String(data));
    }
}
