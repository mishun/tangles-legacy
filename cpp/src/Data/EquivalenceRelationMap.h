#pragma once

#include <utility>
#include <algorithm>
#include <map>
#include "./nullptr.h"

namespace Data
{
	template<class Key, class Value, class Merger>
	class EquivalenceRelationMap
	{
	private:
		struct RootInfo
		{
			Value value;
			RootInfo * next;
			RootInfo * prev;

			RootInfo(const Value & v)
				: value(v)
				, next(nullptr)
				, prev(nullptr)
			{
			}
		};

		struct Node
		{
			Node * father;
			size_t rank;
			RootInfo * info;

			Node(const Value & v)
				: father(this)
				, rank(0)
				, info(new RootInfo(v))
			{
			}

			Node * findRoot()
			{
				if(father->father != father)
					father = father->findRoot();

				return father;
			}
		};

	public:
		class iterator
		{
			friend class EquivalenceRelationMap;

		private:
			RootInfo * info;

		private:
			explicit iterator(RootInfo * n)
				: info(n)
			{
			}

		public:
			iterator(const iterator & that)
				: info(that.info)
			{
			}

			const iterator & operator++()
			{
				if(info != nullptr)
					info = info->next;
				return *this;
			}

			const Value & operator*() const
			{
				assert(info != nullptr);
				return info->value;
			}

			const Value * operator->() const
			{
				assert(info != nullptr);
				return &info->value;
			}

			bool operator==(const iterator & that) const
			{
				return info == that.info;
			}

			bool operator!=(const iterator & that) const
			{
				return info != that.info;
			}
		};

	private:
		Merger merger;
		RootInfo * head;
		RootInfo * tail;
		std::map<Key, Node *> nodes;

	public:
		EquivalenceRelationMap()
			: merger()
			, head(nullptr)
			, tail(nullptr)
			, nodes()
		{
		}

		~EquivalenceRelationMap()
		{
			for(auto it = nodes.begin(); it != nodes.end(); ++it)
			{
				if(it->second->info != nullptr)
					delete it->second->info;

				delete it->second;
			}
		}

		bool insert(const Key & key, const Value & value)
		{
			if(nodes.count(key) > 0)
				return false;

			nodes.insert(std::make_pair(key, addNode(value)));
			return true;
		}

		bool contains(const Key & key) const
		{
			return nodes.count(key) > 0;
		}

		void merge(const Key & key, const Value & value)
		{
			auto it = nodes.find(key);
			if(it == nodes.end())
				nodes.insert(std::make_pair(key, addNode(value)));
			else
			{
				RootInfo * info = it->second->findRoot()->info;
				info->value = merger(info->value, value);
			}
		}

		Value & operator[](const Key & key)
		{
			auto it = nodes.find(key);
			assert(it != nodes.end());
			return it->second->findRoot()->info->value;
		}

		const Value & operator[](const Key & key) const
		{
			auto it = nodes.find(key);
			assert(it != nodes.end());
			return it->second->findRoot()->info->value;
		}

		bool join(const Key & a, const Key & b)
		{
			auto ai = nodes.find(a);
			if(ai == nodes.end())
				return false;

			auto bi = nodes.find(b);
			if(bi == nodes.end())
				return false;

			return unionTrees(ai->second, bi->second);
		}

		iterator begin() const
		{
			return iterator(head);
		}

		iterator end() const
		{
			return iterator(nullptr);
		}

	private:
		bool unionTrees(Node * a, Node * b)
		{
			a = a->findRoot();
			b = b->findRoot();

			if(a == b)
				return false;

			if(b->rank > a->rank)
				std::swap(a, b);
			else if(a->rank == b->rank)
				a->rank++;

			b->father = a;
			a->info->value = merger(a->info->value, b->info->value);

			if(b->info->next == nullptr)
				tail = b->info->prev;
			else
				b->info->next->prev = b->info->prev;

			if(b->info->prev == nullptr)
				head = b->info->next;
			else
				b->info->prev->next = b->info->next;

			delete b->info;
			b->info = nullptr;
			
			return true;
		}

		Node * addNode(const Value & value)
		{
			Node * node = new Node(value);

			if(tail == nullptr)
				head = tail = node->info;
			else
			{
				node->info->prev = tail;
				tail->next = node->info;
				tail = node->info;
			}

			return node;
		}
	};
}
