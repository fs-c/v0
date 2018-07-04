#include <windows.h>
#include "GameManager.h"
#include "Offsets.h"
#include "Memory.h"
#include "Camera.h"
#include "GameRenderer.h"

namespace Engine
{
	Array<Entity*> GameManager::GetEntities()
	{
		return *Memory::Ptr<Array<Entity*>*>(this, OFFSET_GAMEMANAGER_ENTITYLIST);
	}

	GameManager* GameManager::GetInstance()
	{
		return *reinterpret_cast<GameManager**>(ADDRESS_GAMEMANAGER + reinterpret_cast<uint64_t>(GetModuleHandle(NULL)));
	}

	Entity* GameManager::GetLocalPlayer()
	{
		auto base = GetAddress(ADDRESS_INTERFACE);

		if (!!Memory::IsValidPtr(base))
			return NULL;

		auto ptr1 = *Memory::Ptr<void**>(base, INTERFACE_PTR);

		if (!Memory::IsValidPtr(ptr1))
			return NULL;

		auto ptr2 = *Memory::Ptr<void**>(ptr1, INTERFACE_L1);

		if (!Memory::IsValidPtr(ptr2))
			return NULL;

		auto ptr3 = *Memory::Ptr<void**>(ptr2, INTERFACE_L2);

		if (!Memory::IsValidPtr(ptr3))
			return NULL;

		return Memory::Ptr<Entity*>(ptr3, INTERFACE_L3);
	}

	bool GameManager::IsInGame()
	{
		auto base = GetAddress(ADDRESS_STATUSMANAGER);

		if (!Memory::IsValidPtr(base))
			return false;

		auto check = *Memory::Ptr<bool*>(base, OFFSET_MISC_INGAME);

		return check;
	}

	uintptr_t* GameManager::GetAddress(uintptr_t address)
	{
		return *reinterpret_cast<uintptr_t**>(address + reinterpret_cast<uint64_t>(GetModuleHandle(NULL)));
	}
}