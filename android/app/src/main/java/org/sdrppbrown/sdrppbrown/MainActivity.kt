package org.sdrppbrown.sdrppbrown

import android.Manifest
import android.app.NativeActivity
import android.app.PendingIntent
import android.app.PendingIntent.FLAG_MUTABLE
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.ActivityInfo
import android.content.res.AssetManager
import android.hardware.Sensor
import android.hardware.SensorEvent
import android.hardware.SensorEventListener
import android.hardware.SensorManager
import android.hardware.usb.UsbDevice
import android.hardware.usb.UsbDeviceConnection
import android.hardware.usb.UsbManager
import android.media.AudioManager
import android.media.ToneGenerator
import android.net.Uri
import android.os.Build
import android.os.Bundle
import android.os.Environment
import android.os.VibrationEffect
import android.os.Vibrator
import android.provider.Settings
import android.util.Log
import android.view.KeyEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import androidx.core.app.ActivityCompat
import androidx.core.content.PermissionChecker
import java.io.File
import java.io.FileOutputStream
import java.util.Locale
import java.util.concurrent.LinkedBlockingQueue


private const val ACTION_USB_PERMISSION = "org.sdrppbrown.sdrppbrown.USB_PERMISSION";

class MainActivity : NativeActivity(), SensorEventListener {
    private lateinit var toneG: ToneGenerator
    private lateinit var thisCacheDir: String
    private val TAG: String = "SDR++Brown";
    public var usbManager: UsbManager? = null;
    public var SDR_device: UsbDevice? = null;
    public var SDR_conn: UsbDeviceConnection? = null;
    public var SDR_VID: Int = -1;
    public var SDR_PID: Int = -1;
    public var SDR_FD: Int = -1;
    public var density: Float = 1.0f;

    private val usbReceiver = object : BroadcastReceiver() {
        var devList: HashMap<String, UsbDevice> = HashMap<String, UsbDevice>();
        var devListRequested = 1;

        override fun onReceive(context: Context, intent: Intent) {
            Log.w("SDR++", intent.action.toString())
            if (ACTION_USB_PERMISSION == intent.action) {
                Log.w("SDR++", "USB Permission on receive")
                synchronized(this) {
                    var _this = context as MainActivity;
//                _this.SDR_device = intent.getParcelableExtra(UsbManager.EXTRA_DEVICE, UsbDevice::class.java)
                    _this.SDR_device = intent.getParcelableExtra(UsbManager.EXTRA_DEVICE)
                    if (intent.getBooleanExtra(UsbManager.EXTRA_PERMISSION_GRANTED, false)) {
                        Log.w("SDR++", "USB Permission granted")
                        _this.SDR_conn = _this.usbManager!!.openDevice(_this.SDR_device);

                        // Save SDR info
                        _this.SDR_VID = _this.SDR_device!!.getVendorId();
                        _this.SDR_PID = _this.SDR_device!!.getProductId()
                        _this.SDR_FD = _this.SDR_conn!!.getFileDescriptor();
                    } else {
                        Log.w("SDR++", "USB Permission non-granted")
                    }

                    // Whatever the hell this does
                    context.unregisterReceiver(this);

                    // Hide again the system bars
                    _this.hideSystemBars();
                }

//                devListRequested++;
//                if (devListRequested <= devList.size) {
//                    usbManager!!.requestPermission(devList[devListRequested-1], makePermissionIntent());
//                }

            }
        }
    }


    fun checkAndAsk(permission: String) {
        if (PermissionChecker.checkSelfPermission(
                this,
                permission
            ) != PermissionChecker.PERMISSION_GRANTED
        ) {
            ActivityCompat.requestPermissions(this, arrayOf(permission), 1);
        }
    }

    public fun hideSystemBars() {
        val decorView = getWindow().getDecorView();
        val uiOptions = View.SYSTEM_UI_FLAG_HIDE_NAVIGATION or View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY;
        decorView.setSystemUiVisibility(uiOptions);
    }

    var playStarted = 0L

    var recentz = ArrayList<Float>();

    var compx : Float = 0.0f
    var compy : Float = 0.0f
    var compz : Float = 0.0f
    var accx : Float = 0.0f
    var accy : Float = 0.0f
    var accz : Float = 0.0f

    override fun onSensorChanged(event: SensorEvent?) {
        if (event?.sensor?.type == Sensor.TYPE_MAGNETIC_FIELD) {
            compx = event.values[0]
            compy = event.values[1]
            compz = event.values[2]
//            Log.w("SDR++", "Sensors: $accx $accy $accz $compx $compy $compz")
        }
        if (event?.sensor?.type == Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED) {
            compx = event.values[0]
            compy = event.values[1]
            compz = event.values[2]
//            Log.w("SDR++", "Sensors: $accx $accy $accz $compx $compy $compz")
        }
        if (event?.sensor?.type == Sensor.TYPE_ACCELEROMETER) {
            accx = event.values[0]
            accy = event.values[1]
            accz = event.values[2]
            if (recentz.size > 0) {
                val prev = recentz[recentz.size-1]
                if (Math.abs(prev) < 0.5 ) {
                    if (accz - prev > 0.5) {
                        if (System.currentTimeMillis() > playStarted + 70) {
                            playStarted = System.currentTimeMillis()
//                            toneG.startTone(ToneGenerator.TONE_DTMF_1, 50)
                        }
                    }
                    if (accz - prev < -0.5) {
                        if (System.currentTimeMillis() > playStarted + 170) {
                            playStarted = System.currentTimeMillis()
//                            toneG.startTone(ToneGenerator.TONE_DTMF_2, 150)
                        }
                    }
                }
            }
            recentz.add(accz);
            if (recentz.size > 50) {
                recentz.removeAt(0);
            }

//            Log.w("SDR++", "Sensors: $accx $accy $accz $compx $compy $compz")


            // handle accelerometer values
        }
    }

    override fun onAccuracyChanged(sensor: Sensor?, accuracy: Int) {
    }

    private lateinit var sensorManager: SensorManager
    private var accelerometer: Sensor? = null
    private var compass: Sensor? = null

    override fun onDestroy() {
        super.onDestroy()
        sensorManager.unregisterListener(this)
    }

    public override fun onCreate(savedInstanceState: Bundle?) {
        // Hide bars
        hideSystemBars();

        sensorManager = getSystemService(Context.SENSOR_SERVICE) as SensorManager
        accelerometer = sensorManager.getDefaultSensor(Sensor.TYPE_ACCELEROMETER)
        accelerometer?.let { sensorManager.registerListener(this, it, SensorManager.SENSOR_DELAY_FASTEST) }

        compass = sensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED)
        if (compass != null) {
            sensorManager.registerListener(this, compass, SensorManager.SENSOR_DELAY_FASTEST)
        } else {
            compass = sensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD)
            if (compass != null) {
                sensorManager.registerListener(this, compass, SensorManager.SENSOR_DELAY_FASTEST)
            }
        }
        compass?.let { sensorManager.registerListener(this, it, SensorManager.SENSOR_DELAY_FASTEST) }
        compass = sensorManager.getDefaultSensor(Sensor.TYPE_MAGNETIC_FIELD_UNCALIBRATED)
        compass?.let { sensorManager.registerListener(this, it, SensorManager.SENSOR_DELAY_FASTEST) }

        toneG = ToneGenerator(AudioManager.STREAM_ACCESSIBILITY, 100)




        val dm = resources.displayMetrics
        density = dm.density;
        Log.w("SDR++", "Display density: ${dm.density} scaled: ${dm.scaledDensity} densityDpi: ${dm.densityDpi} width: ${dm.widthPixels} height: ${dm.heightPixels} xdpi: ${dm.xdpi} ydpi: ${dm.ydpi} widthDp: ${dm.widthPixels/dm.density} heightDp: ${dm.heightPixels/dm.density}")

        requestedOrientation = ActivityInfo.SCREEN_ORIENTATION_USER_LANDSCAPE;

        // Ask for required permissions, without these the app cannot run.
        checkAndAsk(Manifest.permission.WRITE_EXTERNAL_STORAGE);
        checkAndAsk(Manifest.permission.READ_EXTERNAL_STORAGE);

        // TODO: Have the main code wait until these two permissions are available

        // Register events
        usbManager = getSystemService(USB_SERVICE) as UsbManager;
        val filter = IntentFilter(ACTION_USB_PERMISSION)
        registerReceiver(usbReceiver, filter)


        // Get permission for all USB devices
        usbReceiver.devList = usbManager!!.getDeviceList();
//        Log.w("SDR++", "Dev list size: " + devList.size.toString())

        val permissionIntent = makeUsbPermissionIntent()

        for ((name, dev) in usbReceiver.devList) {
            Log.w("SDR++", "Dev list item: $name $dev")
            val prodName = dev.productName?.toUpperCase(Locale.US) ?: ""
            if (prodName.indexOf("Network") != -1
                || prodName.indexOf("LAN") != -1
                || prodName.indexOf("KEYBOARD") != -1
                || prodName.indexOf("MOUSE") != -1
            )
                // ignore LAN device.
                continue
            usbManager!!.requestPermission(dev, permissionIntent);
            break; // next request in the handler.
        }

        // Ask for internet permission
        checkAndAsk(Manifest.permission.INTERNET);
        checkAndAsk(Manifest.permission.RECORD_AUDIO);

        val externalStorageDirectory = Environment.getExternalStorageDirectory()
        val testFile = File(externalStorageDirectory, "sdrppbrown.test")
        var success = false;
        try {
            testFile.delete()
            success = testFile.createNewFile()
        } catch (e: java.lang.Exception) {
            // no luck
        }
        if (success) {
            testFile.delete()
        } else {
            val intent = Intent(Settings.ACTION_MANAGE_APP_ALL_FILES_ACCESS_PERMISSION)
            val uri: Uri = Uri.fromParts("package", packageName, null)
            intent.data = uri
            startActivityForResult(intent, 0);
        }

        // temporary directory for C++
        val cacheDir = cacheDir
        this.thisCacheDir = cacheDir.absolutePath

        super.onCreate(savedInstanceState)
    }

    private fun makeUsbPermissionIntent(): PendingIntent? {
        try {
            return PendingIntent.getBroadcast(
                this@MainActivity,
                0,
                Intent(ACTION_USB_PERMISSION),
                0
            )
        } catch (e: Exception) {
            return PendingIntent.getBroadcast(
                this@MainActivity,
                0,
                Intent(ACTION_USB_PERMISSION),
                FLAG_MUTABLE
            )
        }
    }

    fun getThisCacheDir(): String {
        return thisCacheDir;
    }

    fun getDisplayDensityStr(): String {
        return this.density.toString();
    }


    public override fun onResume() {
        // Hide bars again
        hideSystemBars();
        super.onResume();
    }

    fun showSoftInput() {
        val inputMethodManager = getSystemService(INPUT_METHOD_SERVICE) as InputMethodManager;
        inputMethodManager.showSoftInput(window.decorView, 0);
    }

    fun hideSoftInput() {
        val inputMethodManager = getSystemService(INPUT_METHOD_SERVICE) as InputMethodManager;
        inputMethodManager.hideSoftInputFromWindow(window.decorView.windowToken, 0);
        hideSystemBars();
    }

    // Queue for the Unicode characters to be polled from native code (via pollUnicodeChar())
    private var unicodeCharacterQueue: LinkedBlockingQueue<Int> = LinkedBlockingQueue()

    // We assume dispatchKeyEvent() of the NativeActivity is actually called for every
    // KeyEvent and not consumed by any View before it reaches here
    override fun dispatchKeyEvent(event: KeyEvent): Boolean {
        if (event.action == KeyEvent.ACTION_DOWN) {
            unicodeCharacterQueue.offer(event.getUnicodeChar(event.metaState))
        }
        return super.dispatchKeyEvent(event)
    }

    fun pollUnicodeChar(): Int {
        return unicodeCharacterQueue.poll() ?: 0
    }

    public fun createIfDoesntExist(path: String) {
        // This is a directory, create it in the filesystem
        var folder = File(path);
        var success = true;
        if (!folder.exists()) {
            success = folder.mkdirs();
        }
        if (!success) {
            Log.e(TAG, "Could not create folder with path " + path);
        }
    }

    public fun extractDir(aman: AssetManager, local: String, rsrc: String): Int {
        val flist = aman.list(rsrc);
        var ecount = 0;
        if (flist != null) {
            for (fp in flist) {
                val lpath = local + "/" + fp;
                val rpath = rsrc + "/" + fp;

                Log.w(TAG, "Extracting '" + rpath + "' to '" + lpath + "'");

                // Create local path if non-existent
                createIfDoesntExist(local);

                // Create if directory
                val ext = extractDir(aman, lpath, rpath);

                // Extract if file
                if (ext == 0) {
                    // This is a file, extract it
                    val _os = FileOutputStream(lpath);
                    val _is = aman.open(rpath);
                    val ilen = _is.available();
                    var fbuf = ByteArray(ilen);
                    _is.read(fbuf, 0, ilen);
                    _os.write(fbuf);
                    _os.close();
                    _is.close();
                }

                ecount++;
            }
        }
        return ecount;
    }

    fun getArchitectureString(): String {
        return System.getProperty("os.arch");
    }

    fun getAppDir(): String {
        val fdir = getFilesDir().getAbsolutePath();

        // Extract all resources to the app directory
        val aman = getAssets();
        extractDir(aman, fdir + "/res", "res");
        createIfDoesntExist(fdir + "/modules");

        return fdir;
    }


    fun performHapticFeedback() {
        val vibrator = getSystemService(VIBRATOR_SERVICE) as Vibrator
        if (vibrator != null) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                vibrator.vibrate(
                    VibrationEffect.createOneShot(
                        100,
                        VibrationEffect.DEFAULT_AMPLITUDE
                    )
                )
            } else {
                // Deprecated in API 26
                vibrator.vibrate(100)
            }
        }
    }
}